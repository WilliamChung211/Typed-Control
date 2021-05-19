#lang typed/racket ;; The example
(begin (define (f (x : (U Number Char))) : Number (if (number? x)  (add1 x) (char->integer x))) (f  #\a))

