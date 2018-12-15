#lang racket

(require "teken-adt.rkt")
(require "helpers.rkt")

;;;;;;;;;;;;;;;;;;
;; Configuratie ;;
;;;;;;;;;;;;;;;;;;

; Moeten veelvouden van 40 zijn.
(define px-venster-breedte 600)
(define px-venster-hoogte  720)

(define px-element-breedte 40)
(define px-element-hoogte 40)

(define elementen-per-rij (/ px-venster-breedte px-element-breedte))
(define elementen-per-kolom (/ px-venster-hoogte px-element-hoogte))

(debug "Field: "elementen-per-rij" x "elementen-per-kolom)

(define element-breedte (/ 1 elementen-per-rij))
(define element-hoogte (/ 1 elementen-per-kolom))

;; Aangezien Dig Dug niet stappen van volledige blokjes maakt maar in 1/10 van een blokje, ...
;; ... moet Dig Dug veel sneller bewegen. Vandaar deze lage getallen.
;; Dit heeft wel als gevolg dat te kleine snelheden niet de verwachte gevolgen kunnen hebben ...
;; ... wanneer de snelheid te dicht bij de delta-tijd van de spel-lus ligt.
;; Stel dat delta-tijd = 20:
;;    Als snelheid = 20, dan zal Dig Dug elke lus bewegen.
;;    Als snelheid = 21, dan zal Dig Dug omd de 2 lussen bewegen. half zo snel dus, wat je niet zouw verwachten.
;; Dit heb ik opgelost door ook Dig-Dug-rest bij te houden die in dit geval elke spel-lus met 1 verhoogt zouw worden (21 - 20).
;; Als (>  Dig-Dug-rest dig-dug-snelheid) zal Dig Dug soms nog een 2de keer zal bewegen in dezelde spel-lus.
;; Hetzelfde geldt voor alle bewegende objecten.

(define dig-dug-snelheid 30)
(define monster-snelheid 35)
(define rots-snelheid 12)
(define harpoen-snelheid 12)
(define respawn-pauze-tijd 1500)  ;; Als je een leven verliest is er een korte pauze voralleer het spel terug begint.
(define vuur-spuw-tijd 2000)
(define vuur-cooldown-tijd 2000)
(define minimum-goggles-time 2000)

(define minimum-aantal-rotsen 5)
(define maximum-aantal-rotsen 7)

(define pooka-kill-punten 100)
(define faygar-kill-punten 200)
(define rots-kill-punten 400)

(define dig-dug-levens 3)

;;;;;;;;;;;;;;;;;;;
;; Initialisatie ;; 
;;;;;;;;;;;;;;;;;;;

; Hier wordt de diepte en hoogte van iedere laag grond berekend.
; Verminder px-venster-hoogte om het effect te zien.
; (Deze waarden mogen niet worden aangepast)
(define bottom-height 1)
(define top-height 2)

(define sky-depth top-height)
(define ground1-depth 0)
(define ground2-depth 0)
(define ground3-depth 0)
(define ground4-depth 0)
(define bottom-row-depth 0)
(let*
    ((min-layer-height (floor (/ (- elementen-per-kolom top-height bottom-height) 4)))
     (remaining-rows (- elementen-per-kolom top-height bottom-height (* 4 min-layer-height)))
     (layers-height (make-vector 4 min-layer-height)))  
  (let distribute-remaing-rows-loop
    ((rows-to-distribute remaining-rows)
     (layer-i 0))
    (if (> rows-to-distribute 0)
        (begin (vector-set! layers-height layer-i (+ min-layer-height 1))
               (distribute-remaing-rows-loop (- rows-to-distribute 1) (+ layer-i 1)))
        (begin (set! ground1-depth (+ top-height (vector-ref layers-height 0)))
               (set! ground2-depth (+ ground1-depth (vector-ref layers-height 1)))
               (set! ground3-depth (+ ground2-depth (vector-ref layers-height 2)))
               (set! ground4-depth (+ ground3-depth (vector-ref layers-height 3)))
               (set! bottom-row-depth (+ ground4-depth bottom-height))))))

;;;;;;;;;;;;;;;;;;;
;; Hulp functies ;;
;;;;;;;;;;;;;;;;;;;

(define (random-x-positie)
  (let ((random-kolom (random elementen-per-rij)))
    (* random-kolom element-breedte)))

(define (random-y-positie)
  (let ((random-rij (random elementen-per-kolom)))
    (* random-rij element-hoogte)))

;; Converteren een coordinaat naar een rij of kolom. Als default wordt floor gebruikt maar eventueel ook ceiling of round.
(define (x->col x [rounding floor])
  (rounding (* x elementen-per-rij)))

(define (y->row y [rounding floor])
  (rounding (* y elementen-per-kolom)))

(define (omgekeerde dir)
  (cond ((eq? dir 'up)  'down)
        ((eq? dir 'down) 'up)
        ((eq? dir 'left) 'right)
        (else 'left)))

(define (volgende-positie x y richting)
  (cond ((and (eq? 'left richting) (> x 0))
         (cons (- x (/ element-breedte 10)) y))
        ((and (eq? 'right richting) (< x (/ (- elementen-per-rij 1) elementen-per-rij)))
         (cons (+ x (/ element-breedte 10)) y))
        ((and (eq? 'up richting) (> y (/ (- sky-depth 1) elementen-per-kolom)))
         (cons x (- y (/ element-hoogte 10))))
        ((and (eq? 'down richting) (< y (/ (- elementen-per-kolom 2) elementen-per-kolom)))
         (cons x (+ y (/ element-hoogte 10))))
        (else (cons x y))))

;; Collision tussen 2 objecten
(define (collision? object-1 object-2)
  (let ((x1 (object-1 'x))
        (x2 (object-2 'x))
        (y1 (object-1 'y))
        (y2 (object-2 'y)))
    (not (or (<= (+ x1 element-breedte) x2)
             (<= (+ x2 element-breedte) x1)
             (<= (+ y1 element-hoogte) y2)
             (<= (+ y2 element-hoogte) y1)))))

;; Collision tussen een object en een positie. Dit wordt gebruikt voor ...
;; ... collisions tussen een object en iets dat niet als een ADT geimplementeerd is.
;; VB: Monster en harpoen. (Harpen is geen ADT, enkel een x en y coordinaat)
(define (collision-object-pos? object x y)
  (let ((dummy-object-dispatch (lambda (msg)
                                 (cond ((eq? msg 'x) x)
                                       ((eq? msg 'y) y)))))
    (collision? object dummy-object-dispatch)))
  

; Produceert een paar van het type (delta-x delta-y) waar elk element een nummer is tussen -1 en 1:
; Dit is dus een soort richtingsvector. (-1 0) betekent naar links bewegen.
(define (random-richting) 
  (cond ((even? (random 1 3)) ; Horizontal:
         (cons 0 (if (even? (random 1 3))
                     1           ; naar rechts
                     -1)))       ; naar links
        (else                 ; Vertical:
         (cons (if (even? (random 1 3))
                     1           ; naar beneden
                     -1) 0))))   ; naar boven

(define (between? x min max)
  (and (>= x min) (<= x max)))


;;;;;;;;;;;;;;;;;
;; Dig-Dug ADT ;;
;;;;;;;;;;;;;;;;;
(define (maak-adt-dig-dug x y)
  (define spawn-x x)
  (define spawn-y y)
  (define richting 'right)
  (define type 'dig-dug)
  (define levens dig-dug-levens)
  (define harpoen? #f)
  (define harpoen-x x)
  (define harpoen-y y)

  (define (x! nieuwe-x)
    (set! x nieuwe-x))
  
  (define (y! nieuwe-y)
    (set! y nieuwe-y))
  
  (define (richting! nieuwe-richting)
    (set! richting nieuwe-richting))

  (define (teken! teken-adt)
    ((teken-adt 'teken-dig-dug!) dispatch-dig-dug))

  (define (verlies-leven! teken-adt)
    (set! levens (- levens 1))
    ((teken-adt 'teken-levens!) levens))

  (define (harpoen! bool)
    (when bool
      (set! harpoen-x x)
      (set! harpoen-y y))
    (set! harpoen? bool))

  (define (schiet-harpoen! speelveld-adt teken-adt)
    (let* ((volgende-pos (volgende-positie harpoen-x harpoen-y richting))
           (volgende-x (car volgende-pos))
           (volgende-y (cdr volgende-pos))
           (next-block (cond ((eq? richting 'down)
                              ((speelveld-adt 'get-block) (x->col volgende-x) (y->row volgende-y ceiling)))
                             ((eq? richting 'right)
                              ((speelveld-adt 'get-block) (x->col volgende-x ceiling) (y->row volgende-y)))
                             (else  ;; Left and Up
                              ((speelveld-adt 'get-block) (x->col volgende-x) (y->row volgende-y))))))
      (if (and (next-block 'tunnel?) (< (abs (- x harpoen-x)) element-breedte) (< (abs (- y harpoen-y)) element-hoogte))
          (begin (set! harpoen-x volgende-x)
                 (set! harpoen-y volgende-y)
                 ((teken-adt 'teken-harpoen!) harpoen-x harpoen-y richting))
          (begin (harpoen! #f) (teken-adt 'remove-harpoen!)))))
      
  (define (beweeg! speelveld-adt teken-adt richting-key)
    (when (not harpoen?) ;; Dig Dug mag enkel bewegen als er geen haropen wordt afgevuurt.
        ;; Dig-Dug kan enkel van richting veranderen als hij zich onveer in het midden van een rij en kolom bevindt.
        ;; Als dit niet het geval is zal hij verder bewegen in de huidige richting.
        (begin (let* ((digdug-center-x (+ x (/ element-breedte 2)))         ;; De coordinaten van het midden van Dug Dug. (tussen 0 en 1)
                      (digdug-center-y (+ y (/ element-hoogte 2)))
                      (digdug-col-center-x (* digdug-center-x elementen-per-rij))  ;; Het midden van Dig Dug in rij/kolom formaat (niet afgerond).
                      (digdug-row-center-y (* digdug-center-y elementen-per-kolom))
                      (digdug-col (floor digdug-col-center-x))                         ;; De index van de rij en colom waar het midden van Dig Dug zich bevind.
                      (digdug-row (floor digdug-row-center-y))
          
                      (delta-block-center-x (abs (- digdug-col-center-x digdug-col 0.5))) ;; De afstand tussen het midden van Dig Dug en het midden van de rij en kolom. (0.5 = En halve block)
                      (delta-block-center-y (abs (- digdug-row-center-y digdug-row 0.5)))
                      
                      (current-block-x (/ digdug-col elementen-per-rij))        ;; De positie (tussen 0 en 1) van de rij en kolom van Dig Dug
                      (current-block-y (/ digdug-row elementen-per-kolom))
           
                      (centered-x? (< delta-block-center-x 0.2))     ;; Dig Dug moet ongeveer in het midden zijn 0.2 (1/5de van een block) is een acceptabele max afstand tussen Dig Dug en het midden.
                      (centered-y? (< delta-block-center-y 0.2)))

                 (cond ((and (member richting-key '(right left))  centered-y?)   ;; Als Dig Dug ongeveer in het midden staat zetten we hem eerst exact in het midden.
                        (y! current-block-y)
                        (richting! richting-key))
                       ((and (member richting-key '(up down)) centered-x?)
                        (x! current-block-x)
                        (richting! richting-key)))

                 (let ((next-block (cond ((eq? richting 'down)
                                          ((speelveld-adt 'get-block) (x->col x) (y->row y ceiling)))
                                         ((eq? richting 'right)
                                          ((speelveld-adt 'get-block) (x->col x ceiling) (y->row y)))
                                         (else  ;; Left and Up
                                          ((speelveld-adt 'get-block) (x->col x) (y->row y))))))
                   
                   (when (and next-block (not (next-block 'tunnel?)) (not (= (next-block 'niveau) 5))) ;; Graaf een tunnel indien nodig.
                     ((next-block 'delete!) teken-adt)))  
      
               (let ((volgende  (volgende-positie x y richting)))
                 (x! (car volgende))
                 (y! (cdr volgende)))
               (teken! teken-adt)))))

  (define (delete! teken-adt)
    (teken-adt 'remove-harpoen!)
    ((teken-adt 'delete-entity!) dispatch-dig-dug))

  (define (reset! teken-adt)
    (teken-adt 'remove-harpoen!)
    (x! spawn-x)
    (y! spawn-y)
    (teken! teken-adt))
  
  (define (dispatch-dig-dug msg)
    (cond ((eq? msg 'x!) x!)
          ((eq? msg 'y!) y!)
          ((eq? msg 'x) x)
          ((eq? msg 'y) y)
          ((eq? msg 'richting) richting)
          ((eq? msg 'richting!) richting!)
          ((eq? msg 'type) type)
          ((eq? msg 'beweeg!) beweeg!)
          ((eq? msg 'teken!) teken!)
          ((eq? msg 'delete!) delete!)
          ((eq? msg 'verlies-leven!) verlies-leven!)
          ((eq? msg 'levens) levens)
          ((eq? msg 'reset!) reset!)
          ((eq? msg 'schiet-harpoen!) schiet-harpoen!)
          ((eq? msg 'harpoen?) harpoen?)
          ((eq? msg 'harpoen!) harpoen!)
          ((eq? msg 'harpoen-x) harpoen-x)
          ((eq? msg 'harpoen-y) harpoen-y)))
  
  dispatch-dig-dug)



;;;;;;;;;;;;;;;;;
;; Pooka ADT ;;
;;;;;;;;;;;;;;;;;
(define (maak-adt-pooka x y)
  (define spawn-x x)
  (define spawn-y y)
  (define richting 'right)
  (define type 'pooka)
  (define goggles? #f)
  (define passed-min-goggles-time? #f) ;; Monsters moeten een minimum aantal seconden in goggles zijn voraleer ze terug normaal worden.
  (define goggles-timer (* 1000 (random 3 10)))

  (define (x! nieuwe-x)
    (set! x nieuwe-x))
  
  (define (y! nieuwe-y)
    (set! y nieuwe-y))
  
  (define (richting! nieuwe-richting)
    (set! richting nieuwe-richting))

  (define (teken! teken-adt)
    (if goggles?
        ((teken-adt 'teken-goggles!) dispatch-pooka)
        ((teken-adt 'teken-pooka!) dispatch-pooka)))

  (define (delete! teken-adt)
    ((teken-adt 'delete-entity!) dispatch-pooka))
  
  (define (beweeg! dig-dug-adt speelveld-adt teken-adt)

    ;; Deze let* is bijna het zelfde als de beweeg! procedure in het Dig-Dug-adt. Zie daar vor uitleg.
    (let* ((center-x (+ x (/ element-breedte 2)))
           (center-y (+ y (/ element-hoogte 2)))
           (col (* center-x elementen-per-rij))
           (row (* center-y elementen-per-kolom))
           (delta-block-center-x (abs (- col (floor col) 0.5)))
           (delta-block-center-y (abs (- row (floor row) 0.5)))
           (current-block-x (/ (x->col x round) elementen-per-rij))
           (current-block-y (/ (y->row y round) elementen-per-kolom))
           (centered-x? (= delta-block-center-x 0))
           (centered-y? (= delta-block-center-y 0)))
      
      (when (and centered-x? centered-y?)
        (let* ((neighboring-blocks ((speelveld-adt 'neighbors) (floor col) (floor row)))
               (valid-directions (if goggles?                      
                                     (filter (lambda (richting) richting)                   ;; Met goggles: Maak een lijst met de richtinen naar Dig Dug. Een beetje intelligent.
                                             (list (cond ((< (dig-dug-adt 'x) x) 'left)
                                                         ((> (dig-dug-adt 'x) x) 'right)
                                                         (else #f))
                                                   (cond ((> (dig-dug-adt 'y) y) 'down)
                                                         ((< (dig-dug-adt 'y) y) 'up)
                                                         (else #f))))
                                     (begin (map car (filter (lambda (direction-and-block)  ;; Geen goggles: Maak een lijst met alle mogelijke richtingen. Monstertjes zullen willekeurig webegen.
                                                               (and (cdr direction-and-block)
                                                                    ((cdr direction-and-block) 'tunnel?)))
                                                             neighboring-blocks)))))
               (reduced-directions (remove (omgekeerde richting) valid-directions))) ;; Alle richting zonder het omgekeerde van de huidige. Dit vermijdt Dat de monstertjes heen en weer bewegen. vb: (links, rechts, links, rechts, ...) is slecht.

          (cond ((= 1 (length valid-directions))  ;; Deze situatie is wanneer het monstertje aan het einde van een tunnel staat. In deze situatie mag een monstertje wel naar de omgekeerde richting gaan.
                 (richting! (car valid-directions)))
                (else                             ;; Als er meedere mogelijke richtingen zijn wordt er een willekeurige gekozen behalve het omgekeerde.
                 (let* ((random-direction (list-ref reduced-directions (random (length reduced-directions)))))
                   (richting! random-direction)
                   (x! current-block-x)
                   (y! current-block-y))))))
      
      (let ((volgende  (volgende-positie x y richting)))
        (x! (car volgende))
        (y! (cdr volgende)))
      
      (teken! teken-adt)
      
      (when (and goggles? passed-min-goggles-time?)
        (let ((current-block ((speelveld-adt 'get-block) (floor col) (floor row))))
          (when (and current-block (current-block 'tunnel?))
            (set! goggles? #f)
            (set! passed-min-goggles-time? #f)
            (set! goggles-timer (* 1000 (random 5 8))))))))

  (define (reset! teken-adt)
    (x! spawn-x)
    (y! spawn-y)
    (teken! teken-adt))

  (define (countdown-goggles! delta-tijd)
    (set! goggles-timer (- goggles-timer delta-tijd))
    (when (< goggles-timer 0)
      (set! goggles? #t))
    (when (< goggles-timer (- minimum-goggles-time))
      (set! passed-min-goggles-time? #t)))
  
  (define (dispatch-pooka msg)
    (cond ((eq? msg 'x!) x!)
          ((eq? msg 'y!) y!)
          ((eq? msg 'x) x)
          ((eq? msg 'y) y)
          ((eq? msg 'beweeg!) beweeg!)
          ((eq? msg 'richting) richting)
          ((eq? msg 'richting!) richting!)
          ((eq? msg 'type) type)
          ((eq? msg 'teken!) teken!)
          ((eq? msg 'delete!) delete!)
          ((eq? msg 'reset!) reset!)
          ((eq? msg 'countdown-goggles!) countdown-goggles!)
          ((eq? msg 'goggles?) goggles?)
          ))
  dispatch-pooka)



;;;;;;;;;;;;;;;;;
;; faygar ADT  ;;
;;;;;;;;;;;;;;;;;
(define (maak-adt-faygar x y)
  (define spawn-x x)
  (define spawn-y y)
  (define richting 'right)
  (define type 'faygar)
  (define vuur-x (+ x element-breedte))
  (define vuur-y y)
  (define vuur-richting 'right)
  (define vuur-cooldown-timer 5000)
  (define vuur-spuw-timer vuur-spuw-tijd)
  (define vuur-active? #f)
  (define goggles? #f)
  (define goggles-timer (* 1000 (random 3 8)))
  (define passed-min-goggles-time? #f)

  (define (x! nieuwe-x)
    (cond ((eq? richting 'right)
           (set! vuur-x (+ nieuwe-x element-breedte)))
          ((eq? richting 'left)
           (set! vuur-x (- nieuwe-x element-breedte ))))
    (set! x nieuwe-x))
    
  (define (y! nieuwe-y)
    (set! vuur-y nieuwe-y)
    (set! y nieuwe-y))
    
  (define (probeer-vuur! teken-adt speelveld-adt dt)
    (when (not goggles?)
      (set! vuur-cooldown-timer (- vuur-cooldown-timer dt)))
    (when (< vuur-cooldown-timer 0)
      (let* ((next-block ((speelveld-adt 'get-next) (x->col x) (y->row y) vuur-richting))
             (next-tunnel? (if next-block (next-block 'tunnel?) #f)))
        
        (cond ((and next-tunnel? (>= vuur-spuw-timer 0) (not goggles?))  ;; Teken vuur
               (when (< vuur-cooldown-timer -100)     
                 (set! vuur-active? #t))
               ((teken-adt 'draw-fire!) dispatch-faygar)
               (set! vuur-spuw-timer (- vuur-spuw-timer dt)))
              ((or (not next-tunnel?) (< vuur-spuw-timer 0) goggles?)  ;; Stop vuur
               ((teken-adt 'remove-fire!) dispatch-faygar)
               (set! vuur-active? #f)
               (set! vuur-cooldown-timer (- vuur-cooldown-tijd vuur-spuw-timer))
               (set! vuur-spuw-timer vuur-spuw-tijd))))))

  (define (beweeg! dig-dug-adt speelveld-adt teken-adt)
    ;; Zelde als Pooka-adt. Zie Pooka-adt voor commentaar.
    (let* ((center-x (+ x (/ element-breedte 2)))
           (center-y (+ y (/ element-hoogte 2)))
           (col (* center-x elementen-per-rij))
           (row (* center-y elementen-per-kolom))
           (delta-block-center-x (abs (- col (floor col) 0.5)))
           (delta-block-center-y (abs (- row (floor row) 0.5)))
           (current-block-x (/ (x->col x round) elementen-per-rij))
           (current-block-y (/ (y->row y round) elementen-per-kolom))
           (centered-x? (= delta-block-center-x 0))
           (centered-y? (= delta-block-center-y 0)))
      
      (when (and centered-x? centered-y?)
        (let* ((neighboring-blocks ((speelveld-adt 'neighbors) (floor col) (floor row)))
               (valid-directions (if goggles?
                                     (filter (lambda (richting) richting)
                                             (list (cond ((< (dig-dug-adt 'x) x) 'left)
                                                         ((> (dig-dug-adt 'x) x) 'right)
                                                         (else #f))
                                                   (cond ((> (dig-dug-adt 'y) y) 'down)
                                                         ((< (dig-dug-adt 'y) y) 'up)
                                                         (else #f))))
                                     (begin (map car (filter (lambda (direction-and-block)
                                                               (and (cdr direction-and-block)
                                                                    ((cdr direction-and-block) 'tunnel?)))
                                                             neighboring-blocks)))))
               ;; Alle mogelijke richtingen
               (reduced-directions (remove (omgekeerde richting) valid-directions)))
          
          (cond ((= 1 (length valid-directions))
                 (richting! (car valid-directions)))
                (else
                 (let* ((random-direction (list-ref reduced-directions (random (length reduced-directions)))))
                   (richting! random-direction)
                   (x! current-block-x)
                   (y! current-block-y))))))
      
      (let ((volgende  (volgende-positie x y richting)))
        (x! (car volgende))
        (y! (cdr volgende)))
    
      (teken! teken-adt)
      
      (when (and goggles? passed-min-goggles-time?)
        (let ((current-block ((speelveld-adt 'get-block) (floor col) (floor row))))
          (when (and current-block (current-block 'tunnel?))
            (set! goggles? #f)
            (set! passed-min-goggles-time? #f)
            (set! goggles-timer (* 1000 (random 5 8))))))))
  
    
  (define (richting! nieuwe-richting)
    (when (or (eq? nieuwe-richting 'right) (eq? nieuwe-richting 'left))
      (set! vuur-richting nieuwe-richting))
    (set! richting nieuwe-richting))

  (define (teken! teken-adt)
    (if goggles?
        ((teken-adt 'teken-goggles!) dispatch-faygar)
        ((teken-adt 'teken-faygar!) dispatch-faygar)))

  (define (delete! teken-adt)
    ((teken-adt 'remove-fire!) dispatch-faygar)
    ((teken-adt 'delete-entity!) dispatch-faygar))

  (define (reset! teken-adt)
    (x! spawn-x)
    (y! spawn-y)
    ((teken-adt 'remove-fire!) dispatch-faygar)
    (teken! teken-adt))

  (define (countdown-goggles! delta-tijd)
    (set! goggles-timer (- goggles-timer delta-tijd))
    (when (< goggles-timer 0)
      (set! goggles? #t))
    (when (< goggles-timer (- minimum-goggles-time))
      (set! passed-min-goggles-time? #t)))

  (define (dispatch-faygar msg)
    (cond ((eq? msg 'x!) x!)
          ((eq? msg 'y!) y!)
          ((eq? msg 'x) x)
          ((eq? msg 'y) y)
          ((eq? msg 'richting) richting)
          ((eq? msg 'richting!) richting!)
          ((eq? msg 'beweeg!) beweeg!)
          ((eq? msg 'type) type)
          ((eq? msg 'teken!) teken!)
          ((eq? msg 'delete!) delete!)
          ((eq? msg 'probeer-vuur!) probeer-vuur!)
          ((eq? msg 'vuur-active?) vuur-active?)
          ((eq? msg 'vuur-x) vuur-x)
          ((eq? msg 'vuur-y) vuur-y)
          ((eq? msg 'vuur-richting) vuur-richting)
          ((eq? msg 'reset!) reset!)
          ((eq? msg 'countdown-goggles!) countdown-goggles!)
          ((eq? msg 'goggles?) goggles?)))
  dispatch-faygar)

;;;;;;;;;;;;;;;
;; Rots ADT  ;;
;;;;;;;;;;;;;;;

(define (maak-adt-rots x y)
  (define richting 'down)
  (define falling? #f)
  (define broken? #f)
  
  (define (x! nieuwe-x)
    (set! x nieuwe-x))
  
  (define (y! nieuwe-y)
    (set! y nieuwe-y))
  
  (define (teken! teken-adt)
    ((teken-adt 'draw-rots!) dispatch-rots))

  (define (beweeg! dig-dug-adt speelveld-adt teken-adt)
    (let* ((volgende  (volgende-positie x y richting))
           (volgende-y (cdr volgende))
           (next-block ((speelveld-adt 'get-next) (x->col x) (y->row volgende-y) richting)))

      (cond ((next-block 'tunnel?)  ;; Een rots mag enkel vallen als er een tunnel onder is.
             (when (and (not falling?) (not (collision? dig-dug-adt next-block)))
               (set! falling? #t))

             (when falling?
               (y! volgende-y)
               (teken! teken-adt)))
             
            ((and (not (next-block 'tunnel?)) falling?) ;; De rots stopt met vallen
             (delete! teken-adt)
             (set! broken? #t)
             (set! falling? #f)))))

  (define (delete! teken-adt)
    ((teken-adt 'remove-rots!) dispatch-rots))

  (define (dispatch-rots msg)
    (cond ((eq? msg 'x!) x!)
          ((eq? msg 'y!) y!)
          ((eq? msg 'x) x)
          ((eq? msg 'y) y)
          ((eq? msg 'beweeg!) beweeg!)
          ((eq? msg 'teken!) teken!)
          ((eq? msg 'delete!) delete!)
          ((eq? msg 'falling?) falling?)
          ((eq? msg 'broken?) broken?)
          ))
  dispatch-rots)
  

;;;;;;;;;;;;;;;
;; Block ADT ;;
;;;;;;;;;;;;;;;
(define (maak-adt-block col row niveau)
  (define positie-x (* element-breedte col))
  (define positie-y (* element-hoogte row))
  (define tunnel? #f)

  (define (teken! teken-adt)
    ((teken-adt 'draw-block!) dispatch-block))

  (define (niveau! n)
    (set! niveau n))

  (define (tunnel! bool)
    (set! tunnel? bool))

  
  (define (delete! teken-adt)
    (tunnel! #t)
    ((teken-adt 'remove-block!) dispatch-block))
  
  
  (define (dispatch-block msg)
    (cond ((eq? msg 'x) positie-x)
          ((eq? msg 'y) positie-y)
          ((eq? msg 'niveau) niveau)
          ((eq? msg 'niveau!) niveau!)
          ((eq? msg 'delete!) delete!)
          ((eq? msg 'tunnel!) tunnel!)
          ((eq? msg 'tunnel?) tunnel?)
          ((eq? msg 'teken!) teken!)))
  dispatch-block)


;;;;;;;;;;;;;;;;;;;
;; Speelveld ADT ;;
;;;;;;;;;;;;;;;;;;;
(define (maak-adt-speelveld)
  (define grond-matrix (vector-map! (lambda (make-proc) (make-proc))
                                    (make-vector elementen-per-rij
                                                 (lambda () (make-vector elementen-per-kolom 'null)))))
  (define tunnel-locations '())

  (define (get-block col row)
    (if (and (between? col 0 (- elementen-per-rij 1)) (between? row 0 (- elementen-per-kolom 1)))
        (vector-ref (vector-ref grond-matrix col) row)
        #f))

  (define (get-left col row)
    (get-block (- col 1) row) )

  (define (get-right col row) 
    (get-block (+ col 1) row))

  (define (get-up col row)
    (get-block col (- row 1)))

  (define (get-down col row)
    (get-block col (+ row 1)))

  (define (get-next col row richting)
    (cond ((eq? richting 'down)
           (get-down col row))
          ((eq? richting 'up)
           (get-up col row))
          ((eq? richting 'right)
           (get-right col row))
          ((eq? richting 'left)
           (get-left col row))))

  (define (neighbors col row)
    (list (cons 'left (get-left col row))
          (cons 'right (get-right col row))
          (cons 'up (get-up col row))
          (cons 'down (get-down col row))))
    
  
  (define (grond-matrix-map f)
    (vector-map
     (lambda (col)
       (vector-map f col))
     grond-matrix))
               
  (define (initialiseer! teken-adt)
    ; Vul de matrix met blokken. (block-adt)
    (let fill-loop
      ((col-i 0)
       (row-i 0))
      (cond ((>= col-i elementen-per-rij)
             (debug "Finished ground init."))
            ((>= row-i elementen-per-kolom)
             (fill-loop (+ col-i 1) 0))
            (else
             (let* ((niveau (cond ((< row-i sky-depth) 0)
                                 ((< row-i ground1-depth) 1)
                                 ((< row-i ground2-depth) 2)
                                 ((< row-i ground3-depth) 3)
                                 ((< row-i ground4-depth) 4)
                                 (else 5)))
                   (new-block (maak-adt-block col-i row-i niveau)))
               ;; De eerste rij boven de grond wordt ook beschoud als een tunnel.
               (when (= row-i (- sky-depth 1))
                 ((new-block 'tunnel!) #t))

               (vector-set! (vector-ref grond-matrix col-i) row-i new-block)
               (fill-loop col-i (+ row-i 1))))))
    
    ;; De omgekeerde T-vormige tunnel waar Dig Dug begint. 
    (let generate-spawn-tunnel
      ((x (floor (/ elementen-per-rij 2)))
       (y sky-depth))
      (cond ((< y (floor (/ elementen-per-kolom 2)))
             ;;(((get-block x y) 'niveau!) 0)
             (((get-block x y) 'tunnel!) #t)
             (generate-spawn-tunnel x (+ y 1)))
            ((<= x (+ 2 (floor (/ elementen-per-rij 2))))
             ;;(((get-block (- x 1) y) 'niveau!) 0)
             (((get-block (- x 1) y) 'tunnel!) #t)
             (generate-spawn-tunnel (+ x 1) y))))

    ;; Lijst met de 4 posities waar tunnels getekent gaan worden.
    ;; Deze posities worden opgeslagen zodat ze later gebruit kunnen worden als posities voor de monstertjes.
    (set! tunnel-locations (shuffle (list (cons (floor (/ elementen-per-rij 4)) (floor (+ (/ elementen-per-kolom 4) top-height)))
                                                (cons (floor (* (/ elementen-per-rij 4) 3)) (floor (* 3 (/ elementen-per-kolom 4))))
                                                (cons (floor (/ elementen-per-rij 4)) (floor (* 3 (/ elementen-per-kolom 4))))
                                                (cons (floor (* (/ elementen-per-rij 4) 3)) (floor (+ (/ elementen-per-kolom 4) top-height))))))

    ;; Deze loop gaat over de posities van de vorige stap en "graaft" tunnels in een willekeurige richting en lengte.
    (let generate-tunnels-outer-loop
      ((remaining-tunnels tunnel-locations))
      (when (not (null? remaining-tunnels))
        (let generate-tunnels-inner-loop
          ((x (car (car remaining-tunnels)))
           (y (cdr (car remaining-tunnels)))
           (richting (random-richting))
           (length (random 6 8)))
          (cond ((and (between? x 0 (- elementen-per-rij 1)) (between? y 3 (- elementen-per-kolom 2)) (> length 0))
                 ;;(((get-block x y) 'niveau!) 0)
                 (((get-block x y) 'tunnel!) #t)
                 (generate-tunnels-inner-loop (+ x (car richting))
                                              (+ y (cdr richting))
                                              richting
                                              (- length 1)))
                (else (generate-tunnels-outer-loop (cdr remaining-tunnels)))))))

    
    ; Alle blocks worden nu getekend.
    (grond-matrix-map (lambda (grond-block) ((grond-block 'teken!) teken-adt))))
  

  (define (remove-all-blocks! teken-adt)
    (grond-matrix-map (lambda (grond-block) ((grond-block 'delete!) teken-adt))))

  
  (define (dispatch-speelveld msg)
    (cond ((eq? msg 'init!) initialiseer!)
          ((eq? msg 'get-block) get-block)
          ((eq? msg 'get-left) get-left)
          ((eq? msg 'get-right) get-right)
          ((eq? msg 'get-up) get-up)
          ((eq? msg 'get-down) get-down)
          ((eq? msg 'get-next) get-next)
          ((eq? msg 'remove-all-blocks!) remove-all-blocks!)
          ((eq? msg 'tunnel-locations) tunnel-locations)
          ((eq? msg 'neighbors) neighbors)))
  
  dispatch-speelveld)

;;;;;;;;;;;;;;
;; Menu ADT ;;
;;;;;;;;;;;;;;
(define (maak-menu-adt)
  (define menu-items '(\play \quit))
  (define selected-index 0)

  (define (start! teken-adt)
    (teken-adt 'draw-menu!))

  (define (close! teken-adt)
    (teken-adt 'delete-menu!))

  (define (select-next! teken-adt)
    (set! selected-index (modulo (+ selected-index 1) (length menu-items)))
    ((teken-adt 'move-menu-selector-to!) (get-selected-item)))
  
  (define (select-previous! teken-adt)
    (set! selected-index (modulo (- (+ selected-index (length menu-items)) 1) (length menu-items)))
    ((teken-adt 'move-menu-selector-to!) (get-selected-item)))

  (define (get-selected-item)
    (list-ref menu-items selected-index))

  (define (handle-key-press key teken-adt)
    (cond ((eq? key 'up)
           (select-previous! teken-adt))
          ((eq? key 'down)
           (select-next! teken-adt))
          (else #f)))

  (define (dispatch-menu msg)
    (cond ((eq? msg 'start!) start!)
          ((eq? msg 'close!) close!)
          ((eq? msg 'handle-key-press) handle-key-press)
          ((eq? msg 'selected-item) (get-selected-item))))
  dispatch-menu)

  
  
;;;;;;;;;;;;;;
;; Spel ADT ;;
;;;;;;;;;;;;;;
(define (maak-adt-spel)
  (define dig-dug-adt #f)
  (define speelveld-adt #f)
  (define teken-adt (maak-adt-teken "Dig Dug " px-venster-hoogte px-venster-breedte px-element-breedte px-element-hoogte))
  (define menu-adt #f)
  (define current-richting-key #f)
  (define respawning #f)
  (define score 0)
  (define round-count 1)
  (define menu-is-open #t)
  (define monsters '())
  (define rotsen '())
  (define respawn-time 0)
  (define highscore (call-with-input-file "highscore.txt"
                      (lambda (in)
                        (string->number (read-line in)))))

  (define (generate-level)
    ((dig-dug-adt 'reset!) teken-adt)
    (set! speelveld-adt (maak-adt-speelveld))
    ((speelveld-adt 'init!) teken-adt)
    ((teken-adt 'teken-round-label!) round-count)
    
    (define aantal-pookas (random 1 4))
    (define aantal-faygars (- 4 aantal-pookas))
    
    (let spawn-monsters
      ((remaining-positions (speelveld-adt 'tunnel-locations))
       (pookas-remaining aantal-pookas)
       (faygars-remaining aantal-faygars))
      (when (not (and (= 0 pookas-remaining) (= 0 faygars-remaining)))
        (let* ((spawn-x (/ (caar remaining-positions) elementen-per-rij))
               (spawn-y (/ (cdar remaining-positions) elementen-per-kolom))
               (monster-adt (cond ((> pookas-remaining 0) (maak-adt-pooka spawn-x spawn-y))
                                ((> faygars-remaining 0) (maak-adt-faygar spawn-x spawn-y))
                                (else #f))))
          ((monster-adt 'teken!) teken-adt)
          (debug "Spawned: "(monster-adt 'type))
          (set! monsters (cons monster-adt monsters))
          (if (eq? (monster-adt 'type) 'pooka)
              (spawn-monsters (cdr remaining-positions) (- pookas-remaining 1) faygars-remaining)
              (spawn-monsters (cdr remaining-positions) pookas-remaining (- faygars-remaining 1))))))

    (let spawn-rotsen
      ((remaining-rotsen (random minimum-aantal-rotsen maximum-aantal-rotsen))
       (used-positions (cons (cons (dig-dug-adt 'x) (dig-dug-adt 'y)) (map (lambda (monster-adt) (cons (monster-adt 'x) (monster-adt 'y))) monsters)))) ;; De posities van de monstertjes en Dig Dug worden beschouwd als bezet.
      (when (> remaining-rotsen 0)
        (let* ((x (random-x-positie))
               (y (random-y-positie))
               (rots-adt (maak-adt-rots x y))
               (block-below ((speelveld-adt 'get-down) (x->col x) (y->row y))))
          (if (and block-below (not (block-below 'tunnel?)) (not (member (cons x y) used-positions))) ;; Je mag enkel rotsen maken boven een plek waar grond is en als die positie nog niet gebruikt is
              (begin ((rots-adt 'teken!) teken-adt)                       
                     (set! rotsen (cons rots-adt rotsen))
                     (spawn-rotsen (- remaining-rotsen 1) (cons (cons x y) used-positions)))
              (spawn-rotsen remaining-rotsen used-positions))))))

  (define (clear-level)
    ((speelveld-adt 'remove-all-blocks!) teken-adt)
    ((dig-dug-adt 'delete!) teken-adt)
    
    (let delete-monsters-loop
      ((remaining-monsters monsters))
      (if (null? remaining-monsters)
          (set! monsters '())
          (begin (((car remaining-monsters) 'delete!) teken-adt)
                 (delete-monsters-loop (cdr remaining-monsters)))))
    
    (let delete-rotsen-loop
      ((remaining-rotsen rotsen))
      (if (null? remaining-rotsen)
          (set! rotsen '())
          (begin (((car remaining-rotsen) 'delete!) teken-adt)
                 (delete-rotsen-loop (cdr remaining-rotsen))))))

  (define (open-menu)
    (set! score 0)
    (set! round-count 1)
    (set! menu-is-open #t)
    (clear-level)
    (set! menu-adt (maak-menu-adt))
    ((menu-adt 'start!) teken-adt)
    (set! dig-dug-adt #f)
    (set! speelveld-adt #f)
    (teken-adt 'remove-scores!)
    (teken-adt 'remove-round-label!))
    
  (define (start-game)
    (set! menu-is-open #f)
    ((menu-adt 'close!) teken-adt)
    
    (set! menu-adt #f)
    (set! dig-dug-adt (maak-adt-dig-dug (/ (floor (/ elementen-per-rij 2)) elementen-per-rij)
                                        (/ (floor (/ elementen-per-kolom 2)) elementen-per-kolom)))
    ((dig-dug-adt 'teken!) teken-adt)
    ((teken-adt 'teken-levens!) (dig-dug-adt 'levens))
    ((teken-adt 'teken-scores!) score highscore)
    (generate-level))
  

  (define (beweeg-rotsen)
    (let loop-over-rotsen
      ((remaining-rotsen rotsen))
      (when (not (null? remaining-rotsen))
        (let ((rots-adt (car remaining-rotsen)))
          (if (rots-adt 'broken?)
              (set! rotsen (remove rots-adt rotsen))
              (begin
                ((rots-adt 'beweeg!) dig-dug-adt speelveld-adt teken-adt)
                ;; Collision tussen rots en Dig Dug
                (let* ((volgende-positie-dig-dug (volgende-positie (dig-dug-adt 'x) (dig-dug-adt 'y) current-richting-key)))
                  (when (collision-object-pos? rots-adt (car volgende-positie-dig-dug) (cdr volgende-positie-dig-dug))
                    (if (rots-adt 'falling?)
                        (begin  ((dig-dug-adt 'verlies-leven!) teken-adt)
                                ((rots-adt 'delete!) teken-adt)
                                (set! rotsen (remove rots-adt rotsen))
                                (reset-positions!))
                        (begin  (set! current-richting-key #f)))))))) ;; Als Dig Dug tegen een niet bewegende rots bots moet Dig Dug stoppen.
        (loop-over-rotsen (cdr remaining-rotsen)))))
  
  (define (beweeg-monsters delta-tijd)
    (let loop-over-monsters
      ((remaining-monsters monsters))
      (when (not (null? remaining-monsters))
        (let ((monster-adt (car remaining-monsters)))
          ((monster-adt 'countdown-goggles!) delta-tijd)
          ((monster-adt 'beweeg!) dig-dug-adt speelveld-adt teken-adt)
          (when (eq? (monster-adt 'type) 'faygar)
            ((monster-adt 'probeer-vuur!) teken-adt speelveld-adt delta-tijd))
          
          ;; Botsing tussen monsters.
          (let collision-between-monsters-loop
            ((other-monsters (filter (lambda (other-enemy) (not (eq? other-enemy monster-adt))) monsters)))
            (when (not (null? other-monsters))
              (let ((other-monster (car other-monsters)))
                (when (collision? monster-adt other-monster)
                  ((monster-adt 'richting!) (omgekeerde (monster-adt 'richting)))
                  ((other-monster 'richting!) (omgekeerde (other-monster 'richting))))
                (collision-between-monsters-loop (cdr other-monsters)))))
          
          ;; Botsing tussen monster en rots.
          (let collision-with-rots-loop
            ((remaining-rotsen rotsen))
            (when (not (null? remaining-rotsen))
              (let ((rots-adt (car remaining-rotsen)))
                (cond ((and (rots-adt 'falling?) (collision? monster-adt rots-adt))
                       ((monster-adt 'delete!) teken-adt)
                       (set! monsters (remove monster-adt monsters))
                       (debug "Killed " (monster-adt 'type))
                       (let* ((col (x->col (monster-adt 'x)))
                              (row (y->row (monster-adt 'y)))
                              (niveau (((speelveld-adt 'get-block) col row) 'niveau)))
                         (set! score (+ score (+ rots-kill-punten (* niveau rots-kill-punten))))
                         ((teken-adt 'teken-scores!) score highscore)))
                      ((and (not (monster-adt 'goggles?)) (collision? monster-adt rots-adt))
                       ((monster-adt 'richting!) (omgekeerde (monster-adt 'richting))))))
              (collision-with-rots-loop (cdr remaining-rotsen))))

          ;; Botsing tussen monster en harpoen
          (when (and (dig-dug-adt 'harpoen?) (collision-object-pos? monster-adt (dig-dug-adt 'harpoen-x) (dig-dug-adt 'harpoen-y)))
            ((monster-adt 'delete!) teken-adt)
            (set! monsters (remove monster-adt monsters))
            (let* ((col (x->col (monster-adt 'x)))
                   (row (y->row (monster-adt 'y)))
                   (niveau (((speelveld-adt 'get-block) col row) 'niveau))
                   (monster-type (monster-adt 'type))
                   (kill-punten (if (eq? monster-type 'pooka) pooka-kill-punten faygar-kill-punten)))
              (set! score (+ score (+ kill-punten (* niveau kill-punten))))
              ((teken-adt 'teken-scores!) score highscore)))
              
          ;; Botsing tussen monster en Dig Dug
          (let ((did-collide? (if (and (eq? (monster-adt 'type) 'faygar) (monster-adt 'vuur-active?))  ;; Als het monster faygar is moet er ook worden geken of Dig Dug tegen het vuut botst.
                                  (or (collision? dig-dug-adt monster-adt) (collision-object-pos? dig-dug-adt (monster-adt 'vuur-x) (monster-adt 'vuur-y)))
                                  (collision? dig-dug-adt monster-adt))))
            (if did-collide?
                (begin ((dig-dug-adt 'verlies-leven!) teken-adt)
                       (if (> (dig-dug-adt 'levens) 0)
                           (reset-positions!)
                           (dig-dug-adt 'delete!)))
                (loop-over-monsters (cdr remaining-monsters))))))))

  (define (reset-positions!)
      ((dig-dug-adt 'reset!) teken-adt)
      (let reset-monsters-loop
        ((remaining-monsters monsters))
        (when (not (null? remaining-monsters))
          (let ((monster-adt (car remaining-monsters)))
            ((monster-adt 'reset!) teken-adt)
            (reset-monsters-loop (cdr remaining-monsters)))))
      (set! respawning #t))
    
  
  (define (toets-functie-tijdens-spel toets)
    (cond (menu-is-open
           (if (eq? toets #\return)
               (let ((selected (menu-adt 'selected-item)))
                 (cond ((eq? selected 'play)
                        (start-game))
                       ((eq? selected 'quit)
                        (exit))))
               ((menu-adt 'handle-key-press) toets teken-adt)))
          ((member toets '(up down left right))
           (set! current-richting-key toets))
          ((eq? toets #\space) ((dig-dug-adt 'harpoen!) #t))
          (else
           (set! current-richting-key #f))))
 

  (define (start)            
    (define dig-dug-tijd 0)
    (define dig-dug-rest 0)
    (define monster-tijd 0)
    (define monster-rest 0)
    (define rots-tijd 0)
    (define rots-rest 0)
    (define harpoen-tijd 0)
    (define harpoen-rest 0)
    (define end-level-timer 0)
    
    (define (spel-lus-functie delta-tijd)
      (cond (respawning        ; Het spel pauzeert even (1500 ms) nadat je een leven verliest. De spel-lus gaat dan in deze tak.
             (set! respawn-time (+ respawn-time delta-tijd))
             (when (> respawn-time respawn-pauze-tijd)
               (set! respawn-time 0)
               (set! respawning #f)))
            
            ((not menu-is-open) ; De "echte" spel-lus.
             (set! dig-dug-tijd  (+ dig-dug-tijd delta-tijd))
             (set! monster-tijd  (+ monster-tijd delta-tijd))
             (set! rots-tijd  (+ rots-tijd delta-tijd))
             (set! harpoen-tijd  (+ harpoen-tijd delta-tijd))
             
             (when (> rots-tijd rots-snelheid)
               (beweeg-rotsen)
               (when (> rots-rest rots-snelheid)
                 (beweeg-rotsen)
                 (set! rots-rest 0))
               (set! rots-rest (+ rots-rest (-  rots-tijd rots-snelheid)))
               (set! rots-tijd 0))
             
             (when (> monster-tijd monster-snelheid)
               (beweeg-monsters delta-tijd)
               (when (> monster-rest monster-snelheid)
                 (beweeg-monsters delta-tijd)
                 (set! monster-rest 0))
               (set! monster-rest (+ monster-rest (- monster-tijd monster-snelheid)))
               (set! monster-tijd 0))

             (when (and (> dig-dug-tijd dig-dug-snelheid) current-richting-key)
               ((dig-dug-adt 'beweeg!) speelveld-adt teken-adt current-richting-key)
               
               (when (> dig-dug-rest dig-dug-snelheid)
                 ((dig-dug-adt 'beweeg!) speelveld-adt teken-adt current-richting-key)
                 (set! dig-dug-rest 0))
               (set! dig-dug-rest (+ dig-dug-rest (-  dig-dug-tijd dig-dug-snelheid)))
               (set! dig-dug-tijd 0))

             (when (and (> harpoen-tijd harpoen-snelheid) (dig-dug-adt 'harpoen?))
               ((dig-dug-adt 'schiet-harpoen!) speelveld-adt teken-adt)
               (when (and (> harpoen-rest harpoen-snelheid) (dig-dug-adt 'harpoen?))
                 ((dig-dug-adt 'schiet-harpoen!) speelveld-adt teken-adt)
                 (set! harpoen-rest 0))
               (set! harpoen-rest (+ harpoen-rest (-  harpoen-tijd harpoen-snelheid)))
               (set! harpoen-tijd 0))
             

             (when (null? monsters)
               (set! end-level-timer (+ end-level-timer delta-tijd))
               (when (> end-level-timer 100)
                 (clear-level)
                 (set! round-count (+ round-count 1))
                 (generate-level)
                 (set! end-level-timer 0)))
             
             (when (<= (dig-dug-adt 'levens) 0)
               (when (> score highscore)
                 (call-with-output-file "highscore.txt"
                   #:exists 'truncate
                   (lambda (out)
                     (debug "New highscore: " score)
                     (set! highscore score)
                     (display score out))))
               (set! end-level-timer (+ end-level-timer delta-tijd))
               (when (> end-level-timer 100) ;; Korte delay voraleer het menu geopend wordt om de tekenaar de tijd te geven om de dingen te verwijderen. (puur esthetisch, niet noodzakelijk)
                 (set! end-level-timer 0)
                 (open-menu)))))) ; Beëindig het spel en keer terug naar het menu.
               


    ((teken-adt 'set-spel-lus-functie!) spel-lus-functie)
    ((teken-adt 'set-toets-functie!) toets-functie-tijdens-spel)
    
    (set! menu-adt (maak-menu-adt))
    ((menu-adt 'start!) teken-adt))

  (define (dispatch-spel msg)
    (cond ((eq? msg 'start) start)))
  
  dispatch-spel)

(define spel (maak-adt-spel))
((spel 'start))
  



