#lang racket
;(begin (define (f (x : (U False Number True))) : Any x ) (f #\a))
;(begin (define (f (x : (U False Number))) : Number (if (boolean? x) 5 (add1 x) ) (f 2)))
;(begin (define (f (x : Any)) : Any (if (number? x)  x x ))  (define (g (x : Any)) : Any (if (number? (f x)) (add1 x) #f) ) (g 2))