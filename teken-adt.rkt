#lang racket
(require "Graphics.rkt")
(require "helpers.rkt")

(provide maak-adt-teken)

(define (maak-adt-teken titel pixels-verticaal pixels-horizontaal px-element-breedte px-element-hoogte)

  ;;;;;;;;;;;;;;;;;;;
  ;;; Configuratie ;;
  ;;;;;;;;;;;;;;;;;;;
  
  (define venster (make-window pixels-horizontaal pixels-verticaal titel))
  ((venster 'set-background!) "black")

  (define title-tile (make-bitmap-tile "Sprites/title.jpg"))
  (define play-tile (make-bitmap-tile "Sprites/play.jpg"))
  (define quit-tile (make-bitmap-tile "Sprites/quit.jpg"))
  
  (define menu-selector-tile (make-bitmap-tile "Sprites/menu-selector.jpg"))

  (define grond-laag (venster 'make-layer))
  (define entities-laag (venster 'make-layer))
  
  (define menu-laag (venster 'make-layer))
  (define block-tiles '())

  (define levens-tiles '())
  
  (define interface-laag (venster 'make-layer))

  (define harpoen-L-tile (make-bitmap-tile "Sprites/harpoen-L.png" "Sprites/harpoen-L-mask.png"))
  (define harpoen-R-tile (make-bitmap-tile "Sprites/harpoen-R.png" "Sprites/harpoen-R-mask.png"))
  (define harpoen-U-tile (make-bitmap-tile "Sprites/harpoen-U.png" "Sprites/harpoen-U-mask.png"))
  (define harpoen-D-tile (make-bitmap-tile "Sprites/harpoen-D.png" "Sprites/harpoen-D-mask.png"))
  (define current-harpoen-tile #f)


  (define ui-laag (venster 'make-layer))
  (define scores-tile (make-tile pixels-horizontaal px-element-hoogte))
  (define round-tile (make-tile pixels-horizontaal 35))
  ((round-tile 'set-y!) (- pixels-verticaal 35))
  ((ui-laag 'add-drawable) scores-tile)
  ((ui-laag 'add-drawable) round-tile)



  ; entities is een associatielijst waar alle monstertjes en digdug adt's geassocieerd
  ; worden met de juiste laag en tile.
  (define entities '())
  (define fire-tiles '())
  ;;;;;;;;;;;;;;;;;;;;;
  ;;; Teken functies ;;
  ;;;;;;;;;;;;;;;;;;;;;

  (define (teken-entity! entity-adt)
    ;"Selecteer" de correcte laag en tiles uit de assosiatielijst entities.
    (let* ((entity-x (* pixels-horizontaal (entity-adt 'x)))
           (entity-y (* pixels-verticaal   (entity-adt 'y)))
           (entity-tile-data (if (assoc entity-adt entities)
                                 ;Het adt zit al in de assosiatielijst dus kan je die gewoon selecteren.
                                 (cdr (assoc entity-adt entities))
                                 ;Het adt zit nog niet in de assosiatielijst. Het is dus de eerste keer dat het getekent wordt.
                                 (let* ((type (entity-adt 'type))
                                        (tiles (cond ((eq? type 'dig-dug) (list (make-bitmap-tile "Sprites/dig-dug-R.png" "Sprites/dig-dug-R-mask.png")
                                                                                (make-bitmap-tile "Sprites/dig-dug-L.png" "Sprites/dig-dug-L-mask.png")))
                                                     ((eq? type 'faygar) (list (make-bitmap-tile "Sprites/faygar-R.jpg" "Sprites/faygar-R-mask.jpg")
                                                                               (make-bitmap-tile "Sprites/faygar-L.jpg" "Sprites/faygar-L-mask.jpg")))
                                                     ((eq? type 'pooka) (list (make-bitmap-tile "Sprites/pooka-R.png" "Sprites/pooka-R-mask.png")
                                                                              (make-bitmap-tile "Sprites/pooka-L.png" "Sprites/pooka-L-mask.png")))
                                                     ))
                                        (tile-sequence (make-tile-sequence tiles))
                                        (new-entity-data (list 'right tile-sequence 'normal (make-bitmap-tile "Sprites/goggles.png" "Sprites/goggles-mask.png"))))
                                   ((entities-laag 'add-drawable) tile-sequence)
                                   (set! entities (cons (cons entity-adt new-entity-data) entities))
                                   new-entity-data)))
           
           (entity-tile-richting (list-ref entity-tile-data 0))
           (entity-tile-sequence (list-ref entity-tile-data 1))
           (mode (list-ref entity-tile-data 2))
           (goggles-tile (list-ref entity-tile-data 3))
           (richting (entity-adt 'richting)))

      (when (eq? mode 'goggles)
        ((entities-laag 'remove-drawable) goggles-tile)
        ((entities-laag 'add-drawable) entity-tile-sequence)
        (set! mode 'normal)
        (set! entities (remove (cons entity-adt entity-tile-data) entities))
        (set! entity-tile-data (list-set entity-tile-data 2 'normal))
        (set! entities (cons (cons entity-adt entity-tile-data) entities))
        )

      ;Update de richting (links of rechts) van de tile
      (when (and (not (eq? entity-tile-richting richting)) (or (eq? richting 'right) (eq? richting 'left)))
        (entity-tile-sequence 'set-next!)
        (if (eq? entity-tile-richting 'right)
            (set! entity-tile-richting 'left)
            (set! entity-tile-richting 'right))

        
        (set! entities (remove (cons entity-adt entity-tile-data) entities))
        (set! entity-tile-data (list-set entity-tile-data 0 entity-tile-richting))
        (set! entities (cons (cons entity-adt entity-tile-data) entities)))
      ;; Update de positie van de tile.
      ((entity-tile-sequence 'set-x!) entity-x)
      ((entity-tile-sequence 'set-y!) entity-y)))

  
  (define (teken-goggles! entity-adt)
    (let ((entity-tile-data (if (assoc entity-adt entities)
                                (cdr (assoc entity-adt entities))
                                #f)))
      (when entity-tile-data
        (let* ((entity-x (* pixels-horizontaal (entity-adt 'x)))
               (entity-y (* pixels-verticaal   (entity-adt 'y)))
               (entity-tile-sequence (list-ref entity-tile-data 1))
               (mode (list-ref entity-tile-data 2))
               (goggles-tile (list-ref entity-tile-data 3)))
          (when (eq? mode 'normal)
            ((entities-laag 'remove-drawable) entity-tile-sequence)
            ((entities-laag 'add-drawable) goggles-tile)
            (set! mode 'goggles)
            (set! entities (remove (cons entity-adt entity-tile-data) entities))
            (set! entity-tile-data (list-set entity-tile-data 2 'goggles))
            (set! entities (cons (cons entity-adt entity-tile-data) entities))
            )
          ((goggles-tile 'set-x!) entity-x)
          ((goggles-tile 'set-y!) entity-y))
          
          

        )
      )
    )


  (define (delete-entity! entity-adt)
    (let ((entity-tile-data (if (assoc entity-adt entities)
                                (cdr (assoc entity-adt entities))
                                #f)))
      (when entity-tile-data
        (let* ((mode (list-ref entity-tile-data 2))
               (tile-to-remove (if (eq? mode 'normal)
                                   (list-ref entity-tile-data 1)
                                   (list-ref entity-tile-data 3))))
          ((entities-laag 'remove-drawable) tile-to-remove)
          (set! entities (remove (cons entity-adt entity-tile-data) entities)))))
      )


  (define (draw-rots! rots-adt)
    (let ((x (* pixels-horizontaal (rots-adt 'x)))
          (y (* pixels-verticaal   (rots-adt 'y)))
          (rots-tile (if (assoc rots-adt entities)
                         (cdr (assoc rots-adt entities))
                         (let ((new-tile (make-bitmap-tile "Sprites/rots.png" "Sprites/rots-mask.png")))
                           ((entities-laag 'add-drawable) new-tile)
                           (set! entities (cons (cons rots-adt new-tile) entities))
                           new-tile)))
          )
      ((rots-tile 'set-x!) x)
      ((rots-tile 'set-y!) y)
      ))

  (define (remove-rots! rots-adt)
    (let ((rots-to-remove (if (assoc rots-adt entities)
                              (cdr (assoc rots-adt entities))
                               #f)))
      (when rots-to-remove
        ((entities-laag 'remove-drawable) rots-to-remove)
        (set! entities (remove (cons rots-adt rots-to-remove) entities)))))
    
    
  
  (define (draw-block! block-adt)
    (let* ((x (* pixels-horizontaal (block-adt 'x)))
           (y (* pixels-verticaal   (block-adt 'y)))
           (niveau (block-adt 'niveau))
           (tunnel? (block-adt 'tunnel?)))
      (when (and (not tunnel?) (not (= niveau 0)))
        (let ((block-tile (cond ((= niveau 1) (make-bitmap-tile "Sprites/ground1.jpg"))
                                ((= niveau 2) (make-bitmap-tile "Sprites/ground2.jpg"))
                                ((= niveau 3) (make-bitmap-tile "Sprites/ground3.jpg"))
                                ((= niveau 4) (make-bitmap-tile "Sprites/ground4.jpg"))
                                ((= niveau 5) (make-bitmap-tile "Sprites/ground5.jpg")))))
          (set! block-tiles (cons (cons block-adt block-tile) block-tiles))
          ((grond-laag 'add-drawable) block-tile)
          ((block-tile 'set-x!) x)
          ((block-tile 'set-y!) y)
          )
        
        )))
  

  (define (remove-block! block-adt)
    (let ((block-to-remove (if (assoc block-adt block-tiles)
                               (cdr (assoc block-adt block-tiles))
                               #f)))
      (when block-to-remove
        ((grond-laag 'remove-drawable) block-to-remove)
        (set! block-tiles (remove (cons block-adt block-to-remove) block-tiles)))))
  

  (define (draw-menu!)
    (debug "Draw menu")
    ((title-tile 'set-y!) (/ pixels-verticaal 10))
    ((title-tile 'set-x!) (- (/ pixels-horizontaal 2) 150))
    ((menu-laag 'add-drawable) title-tile)

    ((play-tile 'set-y!) (+ (/ pixels-verticaal 10) 100))
    ((play-tile 'set-x!) (- (/ pixels-horizontaal 2) px-element-breedte))
    ((menu-laag 'add-drawable) play-tile)

    ((menu-selector-tile 'set-y!) (play-tile 'get-y))
    ((menu-selector-tile 'set-x!) (- (play-tile 'get-x) (* 2 px-element-breedte)))
    ((menu-laag 'add-drawable) menu-selector-tile)
    
    ((quit-tile 'set-y!) (+ (play-tile 'get-y) px-element-breedte 10))
    ((quit-tile 'set-x!) (- (/ pixels-horizontaal 2) px-element-breedte))
    
    ((menu-laag 'add-drawable) quit-tile))
  

  (define (move-menu-selector-to! menu-item)
    (cond ((eq? menu-item 'play)
           ((menu-selector-tile 'set-y!) (play-tile 'get-y))
           ((menu-selector-tile 'set-x!) (- (play-tile 'get-x) (* 2 px-element-breedte))))
          ((eq? menu-item 'quit)
           ((menu-selector-tile 'set-y!) (quit-tile 'get-y))
           ((menu-selector-tile 'set-x!) (- (quit-tile 'get-x) (* 2 px-element-breedte))))
          (else (debug "Invalid menu-item"))))


  (define (delete-menu!)
    ((menu-laag 'remove-drawable) title-tile)
    ((menu-laag 'remove-drawable) play-tile)
    ((menu-laag 'remove-drawable) quit-tile)
    ((menu-laag 'remove-drawable) menu-selector-tile))
  

  ; Vuur
  (define (draw-fire! faygar-adt)
    (let* ((x (* pixels-horizontaal (faygar-adt 'vuur-x)))
           (y (* pixels-verticaal   (faygar-adt 'vuur-y)))
           (richting (faygar-adt 'vuur-richting))
           (fire-tiles-data (if (assoc faygar-adt fire-tiles)
                                (cdr (assoc faygar-adt fire-tiles))
                                (let* ((tiles (list (cons 'left (make-tile-sequence (list (make-bitmap-tile "Sprites/fire/fire-L.png" "Sprites/fire/fire-L-mask.png")
                                                                                          (make-bitmap-tile "Sprites/fire/fire-L.png" "Sprites/fire/fire-L-mask.png")
                                                                                          (make-bitmap-tile "Sprites/fire/fire2-L.png" "Sprites/fire/fire2-L-mask.png")
                                                                                          (make-bitmap-tile "Sprites/fire/fire2-L.png" "Sprites/fire/fire2-L-mask.png"))))
                                                    (cons 'right (make-tile-sequence (list (make-bitmap-tile "Sprites/fire/fire-R.png" "Sprites/fire/fire-R-mask.png")
                                                                                           (make-bitmap-tile "Sprites/fire/fire-R.png" "Sprites/fire/fire-R-mask.png")
                                                                                           (make-bitmap-tile "Sprites/fire/fire2-R.png" "Sprites/fire/fire2-R-mask.png")
                                                                                           (make-bitmap-tile "Sprites/fire/fire2-R.png" "Sprites/fire/fire2-R-mask.png"))))))
                                       
                                       (tile-data (list richting #f tiles)))
                                  (set! fire-tiles (cons (cons faygar-adt tile-data) fire-tiles))
                                  tile-data)
                                  ))
           (current-tile-richting (list-ref fire-tiles-data 0))
           (current-fire-active? (list-ref fire-tiles-data 1))
           (tile-sequences (list-ref fire-tiles-data 2))
           (current-sequence (cdr (assoc current-tile-richting tile-sequences)))
           (to-draw-sequence (cdr (assoc richting tile-sequences))))

      (when (not current-fire-active?)
        ;; First draw
        ((entities-laag 'add-drawable) to-draw-sequence)
        (set! current-fire-active? #t)
        (set! fire-tiles (remove (cons faygar-adt fire-tiles-data) fire-tiles))
        (set! fire-tiles-data (list-set fire-tiles-data 1 #t))  
        (set! fire-tiles (cons (cons faygar-adt fire-tiles-data) fire-tiles)))
      
      ;; Update de richting (links of rechts) van de tile
      (when (not (eq? current-tile-richting richting))
        (set! current-tile-richting richting)
        ((entities-laag 'remove-drawable) current-sequence)
        ((entities-laag 'add-drawable) to-draw-sequence)
        (set! current-sequence to-draw-sequence)
        (set! fire-tiles (remove (cons faygar-adt fire-tiles-data) fire-tiles))
        (set! fire-tiles-data (list-set fire-tiles-data 0 current-tile-richting))
        (set! fire-tiles (cons (cons faygar-adt fire-tiles-data) fire-tiles)))
      
      (current-sequence 'set-next!)
      ((current-sequence 'set-x!) x)
      ((current-sequence 'set-y!) y)))
  
(define (remove-fire! faygar-adt)
  (let ((fire-tiles-data (if (assoc faygar-adt fire-tiles)
                             (cdr (assoc faygar-adt fire-tiles))
                             #f)))
    (when fire-tiles-data
      (let* ((sequences (list-ref fire-tiles-data 2))
             (sequence-richting (list-ref fire-tiles-data 0))
             (to-remove (cdr (assoc sequence-richting sequences))))
        ((entities-laag 'remove-drawable) to-remove)
        ))))
    

  (define (remove-leven!)
    (when (> (length levens-tiles) 0)
      (let ((last-tile (last levens-tiles)))
        ((interface-laag 'remove-drawable) last-tile)
        (set! levens-tiles (remove last-tile levens-tiles)))))
    
  
  (define (teken-levens! levens)
    (if (< levens (length levens-tiles))
        (remove-leven!)
        (let teken-loop
          ((i (- levens 1)))
          (when (>= i 0)
            (let ((leven-tile (make-bitmap-tile "Sprites/dig-dug-R.png" "Sprites/dig-dug-R-mask.png")))
              ((leven-tile 'set-x!) (* i px-element-breedte))
              ((leven-tile 'set-y!) (- pixels-verticaal px-element-breedte))
              ((interface-laag 'add-drawable) leven-tile)
              (set! levens-tiles (cons leven-tile levens-tiles))
              (teken-loop (- i 1)))))))


  (define (teken-harpoen! x y richting)
    (let ((to-draw-tile (cond ((eq? richting 'right) harpoen-R-tile)
                              ((eq? richting 'left) harpoen-L-tile)
                              ((eq? richting 'up) harpoen-U-tile)
                              ((eq? richting 'down) harpoen-D-tile))))
      (when (not current-harpoen-tile)
        ((entities-laag 'add-drawable) to-draw-tile)
        (set! current-harpoen-tile to-draw-tile))
      ((to-draw-tile 'set-x!) (* pixels-horizontaal x))
      ((to-draw-tile 'set-y!) (* pixels-verticaal y))
      )
    )

  (define (remove-harpoen!)
    (when current-harpoen-tile
      ((entities-laag 'remove-drawable) current-harpoen-tile)
      (set! current-harpoen-tile #f)))
          


  (define (teken-scores! score highscore)
    (scores-tile 'clear)
    ((scores-tile 'draw-text) (string-append "Score: " (number->string score)) 20 0 0 "white")

    (let* ((highscore-string (number->string highscore))
           (highscore-length (string-length highscore-string)))
      ((scores-tile 'draw-text) (string-append "Highscore: " highscore-string) 20 (- pixels-horizontaal 110 (* 15 highscore-length)) 0 "white")))

  (define (remove-scores!)
    (scores-tile 'clear))
  

  (define (teken-round-label! round-count)
    (round-tile 'clear)
    (let* ((round-string (number->string round-count))
           (round-string-length (string-length round-string)))
      ((round-tile 'draw-text) (string-append "Round " round-string) 25 (- pixels-horizontaal 85 (* 25 round-string-length)) 0 "white"))
    )
    
    

  (define (remove-round-label!)
    (round-tile 'clear))
  
  
    
  ;; Spel lus functies
  (define (set-spel-lus-functie! fun)
    ((venster 'set-update-callback!) fun))
  
  (define (set-toets-functie! fun)
    ((venster 'set-key-callback!) fun))

  
  (define (dispatch-teken-adt msg)
    (cond ((eq? msg 'set-toets-functie!) set-toets-functie!)
          ((eq? msg 'set-spel-lus-functie!) set-spel-lus-functie!)
          ;; Teken functies.
          ((eq? msg 'teken-dig-dug!) teken-entity!)
          ((eq? msg 'teken-pooka!) teken-entity!)
          ((eq? msg 'teken-faygar!) teken-entity!)
          ((eq? msg 'delete-entity!) delete-entity!)
          ((eq? msg 'draw-menu!) (draw-menu!))
          ((eq? msg 'delete-menu!) (delete-menu!))
          ((eq? msg 'move-menu-selector-to!) move-menu-selector-to!)
          
          ((eq? msg 'draw-block!) draw-block!)
          ((eq? msg 'remove-block!) remove-block!)

          ((eq? msg 'draw-rots!) draw-rots!)
          ((eq? msg 'remove-rots!) remove-rots!)

          ((eq? msg 'draw-fire!) draw-fire!)
          ((eq? msg 'remove-fire!) remove-fire!)

          ((eq? msg 'teken-levens!) teken-levens!)
          ((eq? msg 'remove-leven!) remove-leven!)

          ((eq? msg 'teken-harpoen!) teken-harpoen!)
          ((eq? msg 'remove-harpoen!) (remove-harpoen!))

          ((eq? msg 'teken-scores!) teken-scores!)
          ((eq? msg 'remove-scores!) (remove-scores!))
          
          ((eq? msg 'teken-round-label!) teken-round-label!)
          ((eq? msg 'remove-round-label!) (remove-round-label!))

          ((eq? msg 'teken-goggles!) teken-goggles!)
          
          
          ;; Functies die interne waarden blootgeven.
          ((eq? msg 'canvas-h) pixels-verticaal)
          ((eq? msg 'canvas-w) pixels-horizontaal)))

  dispatch-teken-adt)
