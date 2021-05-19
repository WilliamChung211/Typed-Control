#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (cond
    [(integer? s) (Int s)]
    [(boolean? s) (Bool s)]
    [else
     (match s  
       [(list (? (op? op1) p1) e)     (Prim1 p1 (parse e))]
       [(list 'if e1 e2 e3)
        (If (parse e1) (parse e2) (parse e3))]
       [_ (error "Parse error")])]))


(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))

(define op1
  '(add1 sub1 zero?))



