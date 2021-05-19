#lang typed/racket ;; A recursive way to multiply someting by 2

(begin (define (f (x : Number)) : Number (if (zero? x) 0 (add1 (add1(f (sub1 x)))) )) (f 7))
