#lang racket

(provide debug)


(define debug? #t)

(define (debug x . xs)
  (when debug?
    (display x)
    (for-each display xs)
    (newline)))
