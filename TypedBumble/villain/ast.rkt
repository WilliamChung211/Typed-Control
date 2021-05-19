#lang racket
(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f bs out_t e) #:prefab)

;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Prim1 Op1 Expr)
;; | (If Expr Expr Expr)
;; type Op1 = 'add1 | 'sub1 | 'zero? | number? | integer?


(struct Int   (i)        #:prefab)
(struct Bool  (b)        #:prefab)
(struct Var   (v)        #:prefab)
(struct Prim1 (p e)      #:prefab)
(struct If    (e1 e2 e3) #:prefab)
(struct App   (f es)       #:prefab)

(struct Binding (x ty) #:prefab)

;; Types


(struct Integer ()          #:prefab)
(struct True ()         #:prefab)
(struct False ()         #:prefab)
(struct Fun (in_type out_type) #:prefab)
(struct Union (ts) #:prefab)

;; represents the any type
(define (any)
  (Union (list (Integer) (True) (False)))
)

;; represents the boolean type
(define (boolean)
  (Union (list (True) (False)))
)

; simplfiies the union by doing a flatmap and getting rid of duplicates
; and making it its own type of it is just a union of one type
(define (simp-union ty)
  (match ty
    [(Union xs) (match (remove-dup (expand xs) #f #f #f)
                  [(list ty) ty]
                  [ ts (Union ts)]
                  )
                ]

    )
  )

; Basically a flat map operation
(define (expand xs)
  (match xs
    ['() '()] 
    [(cons (Union ts) xs) (append (expand ts) (expand xs))]
    [(cons x xs) (cons x (expand xs))]
   )
 )

; removes duplicate types
(define (remove-dup ts has-int has-true has-false)
  (match ts
    ['() '()]
    [(cons (Integer) ts) (if has-int (remove-dup ts has-int has-true has-false) (cons (Integer) (remove-dup ts #t has-true has-false)))]
    [(cons (True) ts) (if has-true (remove-dup ts has-int has-true has-false) (cons (True) (remove-dup ts has-int #t has-false)))]
    [(cons (False) ts) (if has-false (remove-dup ts has-int has-true has-false) (cons (False) (remove-dup ts has-int has-true #t)))]
   )
)