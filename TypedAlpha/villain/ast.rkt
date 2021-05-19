#lang racket
(provide Int Bool Prim1 If Integer Boolean Fun)

;; type Expr =
;; | (Eof)
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Prim1 Op1 Expr)
;; | (If Expr Expr Expr)
;; | (Begin Expr Expr)
;; type Op1 = 'add1 | 'sub1 | 'zero?


(struct Int   (i)        #:prefab)
(struct Bool  (b)        #:prefab)
(struct Prim1 (p e)      #:prefab)
(struct If    (e1 e2 e3) #:prefab)

;; type Type =
;; | (Integer)
;; |  (Boolean)
;; | (Fun Type Type)

(struct Integer ()          #:prefab)
(struct Boolean ()         #:prefab)
(struct Fun (in_type out_type) #:prefab)

