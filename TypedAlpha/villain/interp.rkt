#lang racket
(provide interp)
(require "ast.rkt" "typecheck.rkt" "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  
  (begin (get-type e)
         (match e
           [(Int i)  i]
           [(Bool b) b]
           [(Prim1 p e0)
            (interp-prim1 p (interp e0))]
           [(If e1 e2 e3)
            (if (interp e1)
                (interp e2)
                (interp e3))]
           )
         )
  )
