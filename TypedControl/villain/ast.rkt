#lang racket
(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f bs out_t e) #:prefab)

;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Char Character)
;; | (Var Symbol)
;; | (Prim1 Op1 Expr)
;; | (If Expr Expr Expr)
;; type Op1 = 'add1 | 'sub1 | 'zero?


(struct Int   (i)        #:prefab)
(struct Bool  (b)        #:prefab)
(struct Char  (c)        #:prefab)
(struct Var   (v)        #:prefab)
(struct Prim1 (p e)      #:prefab)
(struct If    (e1 e2 e3) #:prefab)
(struct App   (f es)       #:prefab)


;; Info
(struct Judge (ty thenProp elseProp obj)          #:prefab)

;; Types


(struct Integer ()          #:prefab)
(struct True ()         #:prefab)
(struct False ()         #:prefab)
(struct Character ()     #:prefab)
(struct Fun (in_var in_type out_type thenProp elseProp obj) #:prefab)
(struct Union (ts) #:prefab)
(struct Explosion () #:prefab)

;; Prop

(struct HasType (x ty)  #:prefab)
(struct NotType (x ty)  #:prefab)
(struct OrProp (p1 p2)      #:prefab)
(struct OrPropSimp (x has not) #:prefab)
(struct Tt ()          #:prefab)
(struct Ff ()          #:prefab)


;; Object
(struct Empty() )


;; Syntax

;; this binding is used for parsing functions
(struct Binding (x ty)  #:prefab)

;; this is used to represent the context of the type of variables in the environment
;; expl represens if the entire environment is a contradiction so any variable can be any type
;; typeBs represents what type each variable at least a subtype
(struct TypeEnv (expl typeBs) #:prefab)

;; this is used to represent the context of types of functions defined in the environment
(struct FunEnv (typeBs) #:prefab)

;; this represents that the propsoiton that variable x is a subtype of ty
(struct TypeB (x ty) #:prefab)



;; represents the any type
(define (any)
  (Union (list (Integer) (True) (False) (Character)))
)

;; represents the boolean type
(define (boolean)
  (Union (list (True) (False)))
)

; simplfiies the union by doing a flatmap and getting rid of duplicates
; and making it its own type of it is just a union of one type
(define (simp-union ty)
  (match ty
    [(Union xs) (match (remove-dup (expand xs) #f #f #f #f)
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
(define (remove-dup ts has-int has-true has-false has-char)
  (match ts
    ['() '()]
    [(cons (Integer) ts) (if has-int (remove-dup ts has-int has-true has-false has-char) (cons (Integer) (remove-dup ts #t has-true has-false has-char)))]
    [(cons (True) ts) (if has-true (remove-dup ts has-int has-true has-false has-char) (cons (True) (remove-dup ts has-int #t has-false has-char)))]
    [(cons (False) ts) (if has-false (remove-dup ts has-int has-true has-false has-char) (cons (False) (remove-dup ts has-int has-true #t has-char)))]
    [(cons (Character) ts) (if has-char (remove-dup ts has-int has-true has-false has-char) (cons (Character) (remove-dup ts has-int #t has-false #t)))]
   )
)