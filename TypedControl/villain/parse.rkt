#lang racket
(provide parse)
(require "ast.rkt")


;; S-Expr -> Prog
;; Gets a program which is a number of definitions and the main expression
(define (parse s)
  (match s
    [(list 'begin (and ds (list 'define _ _ _ _)) ... e)
     (Prog (map parse-d ds) (parse-e e))]
    [e (Prog '() (parse-e e))]))

;; S-Expr -> Defn
;; Function definitions must be single parameter functions that are type annotated
(define (parse-d s)
  (match s
    [(list 'define (list (? symbol? f) (list (? symbol? xs) ': inty)) ': outty e)

     ;; Puts a type binding for the input
     (Defn f (list (xs->bs xs (sym->ty inty))) (sym->ty outty) (parse-e e))]
    [_ (error "Parse defn error" s)]))


(define (xs->bs xs inty)
  (Binding xs inty)
  )

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? integer?)                  (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                 (Char s)]
    [(? symbol?)                   (Var s)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(cons (? symbol? f) es)
     (App f (map parse-e es))]
    [_ (error "Parse error" s)]))


(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))

(define op1
  '(add1 sub1 zero? number? boolean? char? char->integer integer->char))

;; Gets the type based on the symbol
(define (sym->ty sym)
  (match sym
    ['Number (Integer)]
    ['True (True)]
    ['False (False)]
    ['Boolean (boolean)]
    ['Char (Character)]
    ['Any (any)]
    [(list 'U xs ...) (simp-union (Union (syms->ty xs)))]
    [_ (error "Parse Error")]
    
    )
  )

;; Handles lists of types from the union list
(define (syms->ty syms)
  (match syms
    ['() '()]
    [(cons s syms) (cons (sym->ty s) (syms->ty syms))]
    )
 )
