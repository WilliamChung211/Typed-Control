#lang racket
(require "ast.rkt" )
(provide interp-prim1)

;; Op1 Value -> Answer
(define (interp-prim1 p1 v)
  (match (list p1 v)
    [(list 'add1 (? integer?))            (add1 v)]
    [(list 'sub1 (? integer?))            (sub1 v)]
    [(list 'zero? (? integer?))           (zero? v)]
    [(list 'integer? v)                   (integer? v)]
    [(list 'boolean? v)                    (boolean? v)]
    [(list 'number? v) (number? v)]
    [(list 'boolean? v) (boolean? v) ]
    [(list 'char? v) (char? v) ]
    [(list 'char->integer v) (char->integer v) ]
    [(list 'integer->char v) (integer->char v) ]
    [_                                    'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
