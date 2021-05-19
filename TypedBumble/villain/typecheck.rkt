#lang racket
(provide get-type)
(require "ast.rkt")


;; gets the type for the program
(define (get-type p)
  (match p
    [(Prog ds e)  (let ((fenv (get-type-funs ds))) (begin (get-type-defines ds '() fenv)(get-type-e e '() fenv)))]
   )
)

;; Gets the environment based on every function definition from the program
(define (get-type-funs ds)
    (match ds
    ['() '()]
    [(cons d ds)
     (list (get-type-fun d)
          (get-type-funs ds))]))
  
;; Gets the type of the function from the type annotation
(define (get-type-fun d)  
  (match d
    [(Defn f (list (Binding x ty)) out_ty _) (Binding f (Fun ty out_ty))]
    )
  )

;; Goes through each function body to check if they passed the type checker
(define (get-type-defines ds env fenv)
    (match ds
    ['() '()]
    [(cons d ds)
     (list (get-type-define d env fenv)
          (get-type-defines ds env fenv))]))
  
;; T-ABS
(define (get-type-define d env fenv)
  
  (match d
    [(Defn f bs out-ty e) (let((e-ty (get-type-e e (append bs env) fenv))) (if

                                                                     ;; checks if the expression of the body matches the intented output type
                                                                     (is-subtype? out-ty e-ty)
                                                                     (Fun (car bs) e-ty)
                                                                     (error (string-append "Type Checker: type mismatch\nexpected: " (print-type out-ty) "\ngiven: " (print-type e-ty)))
                                                                     ))]
     ))

(define (get-type-e e env fenv)
 (match e

   ;; T-NUM
   [(Int i)       (Integer)]
   ;; T-TRUE
   [(Bool #t)      (True)]
   [(Bool #f)      (False)]
   ;; T-VAR
   [(Var v)        (lookup-type v env)]
   ;; T-APP with only constant primitives
   [(Prim1 p e)   (get-type-prim1 p e env fenv)]
   ;; T-IF
   [(If e1 e2 e3) (get-type-if e1 e2 e3 env fenv)]
   ;; T-APP with only functions defined
   [(App f es) (get-type-app f es env fenv)]
   [_ (error e)]))

;; T-CONST 
(define (get-type-op1 p)
 (match p
   ['add1 (Fun (Integer) (Integer)) ]
   ['sub1 (Fun (Integer) (Integer))]
   ['zero? (Fun (Integer) (boolean))]
   ['number? (Fun (any) (boolean))]
   ['boolean? (Fun (any) (boolean))]
   )
  )

;; T-PRIM
(define (get-type-prim1 p e env fenv)

  ;; gets the type of the primitive
 (match (get-type-op1 p)
   [(Fun in-ty out-ty)
    (let ((ty (get-type-e e env fenv)))
      ;; does T-PRIM
     (if (is-subtype? in-ty (get-type-e e env fenv))
         out-ty
          ;; throws an error if it failed the type checking
         (error (string-append "Type Checker: type mismatch\nexpected: " (print-type in-ty) "\ngiven: " (print-type ty)))))]
  )
 )

;; T-IF
;; Gets judgement information of this if expression
(define (get-type-if e1 e2 e3 env fenv)
  (let ((ty (get-type-e e1 env fenv)))

     ;; will return the common supertype of ty2 ty3
    (let ((ty2 (get-type-e e2 env fenv)) (ty3 (get-type-e e3 env fenv)))
      (get-supertype ty2 ty3))

    )
  )

;; T-APP but it must call a function from the environment
(define (get-type-app f es env fenv)
  ( match (lookup-type f fenv)
     [(Fun in-ty out-ty) (let ((e-ty (get-type-e (car es) env fenv)))
      (if (is-subtype? in-ty e-ty)
          out-ty
           (error (string-append "Type Checker: type mismatch\nexpected: " (print-type in-ty) "\ngiven: " (print-type e-ty))))) ]
     )
  )


; HELPER FUNCTIONS


;; returns true if ty2 is a subtype of ty1
(define (is-subtype? ty1 ty2)
  (match ty2

    ;; S-UNIONSUB
    [(Union ts2) (is-subtype-union2? ty1 ts2)]

    [(Integer) (match ty1
               ;; S-UNIONSUPER
               [(Union ts1) (is-subtype-union1? ts1 ty2)]
                 ;; S-REFL
               [(Integer) true]
               [_ false])]
    [(True) (match ty1
              ;; S-UNIONSUPER
                [(Union ts1) (is-subtype-union1? ts1 ty2)]
              ;; S-REFL
               [(True) true]
               [_ false])]
    [(False) (match ty1
               ;; S-UNIONSUPER
               [(Union ts1) (is-subtype-union1? ts1 ty2)]
               ;; S-REFL
               [(False) true]
               [_ false])]

    [_ false]
    )
    
    
  )

;; S-UNIONSUPER
;; returns true if ty2 is a subtype of any type in ts1
(define (is-subtype-union1? ts1 ty2)
  (
   match ts1
    ('() false)
    ((cons ty1 ts1) (or (is-subtype? ty1 ty2) (is-subtype-union1? ts1 ty2)))
     
    )
    
)

;; S-UNIONSUB
;; returns true if everything ts2 is a subtype of t1
(define (is-subtype-union2? ty1 ts2)
  (
   match ts2
    ('() true)
    ((cons ty2 ts2) (and (is-subtype? ty1 ty2) (is-subtype-union2? ty1 ts2)))
    
    )
    
)

(define (lookup-type x t)
  (match t
    ['() error "Syntax error"]
    [(cons (Binding sym ty) ts) (if (equal? x sym) ty (lookup-type x ts))]
    )
  )

;; gets the supertype of the two types which is either a direct supertype, or the union of each other
(define (get-supertype ty1 ty2)
 (if (is-subtype? ty1 ty2)
     ty1
     (if (is-subtype? ty2 ty1)
         ty2
         (simp-union (Union (list ty1 ty2)))
         )
     )
)

;; Used for type checking errors. Prints type
;; Has some non-complete matchings for types like Boolean and Any
(define (print-type ty)
  (match ty
    [(Integer) "Number"]
    [(True) "True"]
    [(False) "False"]
    [(Union (list (True) (False))) "Boolean"]
    [(Union (list (False) (True))) "Boolean"]
    [(Union (list (Integer) (True) (False))) "Any"]
    [(Union xs) (string-append "(U " (print-types xs) ")")]
   )
 )

;; Prints all the types in the list of union
(define (print-types ts)
  (match ts
    [(list ty) (print-type ty)]
    [(cons ty ts) (string-append (print-type ty) " " (print-types ts))]
    )
  )

