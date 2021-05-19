#lang racket
(provide get-judge)
(require "ast.rkt" "typecheckhelp.rkt")

;; TYPING RULES

;; gets the judgement for a program
(define (get-judge p)

  (match p
    [(Prog ds e)  (get-judge-help ds e (TypeEnv #f '()) '())]
   )
)

;; T-BEGIN
(define (get-judge-help ds e env f-env)
  (match ds
    ['() (get-judge-e e env f-env)]
    [(cons d ds)
     (match d
       ;; Context -> e0: type; prop0+ | prop1-; o0
       [(Defn f bs out-ty exp) (match (get-judge-body bs exp env f-env)
                                 [(Judge t0 tprop-0 eprop-0 obj-0)
                                  (match t0
                                    [(Fun _ _ e-ty _ _ _)
                                     (match env
                                       [(TypeEnv expl _)
                                        ;; If there is explosion in the environment, automiatically type checking is pased
                                        ;; Otherwise, what the body returns given the expression must return what the type annotation
                                        ;; it should return or sub type of it
                                        (if (or expl (is-subtype? out-ty e-ty))

                                            
                                            ;; Context with f has type t0 -> e1 : type1; prop1+ | prop1-; o1
                                            ;; in this case e1 is definition representing the rest of the definition bodies and the main expression
                                            (match (get-judge-help ds e env (cons  (TypeB f t0) f-env))
                                              [(Judge t1 tprop-1 eprop-1 obj-1)

                                               ; Context -> (define (f e0) e1): type1 (replace o0 in x); prop1+|prop1- (replace o0 in x); o1 (replace 
                                               (Judge (subs-type t1 obj-0 f) (subs-prop tprop-1 obj-0 f) (subs-prop eprop-1 obj-0 f) (subs-obj obj-1 obj-0 f))]
                                              )

                                            ;; If it fails the type check, it returns an error
                                            (error (string-append "Type Checker: type mismatch\nexpected: " (print-type out-ty) "\ngiven: " (print-type e-ty)))
                                            )])]
                                    )
                                  ]
                                 )]
       
       )]
    )
  )

;; Getting the judgement for an expression using the typing rules
(define (get-judge-e e env f-env)
 (match e

   ;; T-NUM
   [(Int i)       (Judge (Integer) (Tt) (Ff) (Empty))]
   ;; T-TRUE
   [(Bool #t)     (Judge (True) (Tt) (Ff) (Empty))]
   ;; T-FALSE
   [(Bool #f)     (Judge (False) (Ff) (Tt) (Empty))]
   ;; T-CHAR
   [(Char c) (Judge (Character) (Tt) (Ff) (Empty))]
   ;; T-VAR
   [(Var v)        (Judge (lookup-type v env) (NotType v (False)) (HasType v (False)) v)]
   ;; T-APP with only constant primitives
   [(Prim1 p e)   (get-judge-prim1 p e env f-env)]
   ;; T-IF
   [(If e1 e2 e3) (get-judge-if e1 e2 e3 env f-env)]
   ;; T-APP with only functions defined
   [(App f es) (get-judge-app f es env f-env)]
   [_ (error e)]
   )
  )


;; T-ABS
(define (get-judge-body bs e env f-env)
  
  
       (match bs
         [(list (Binding x ty))

          ;; given an environment where the proposition of x hastype ty is added
          (match (get-judge-e e (add-prop (HasType x ty) env) f-env)

            ;; get the judgment information from this expression of the abstraction
            [(Judge e-ty thenProp elseProp Obj)

             ;; return the function type; tt| ff; empty subset as the judgement info
             (Judge (Fun x ty e-ty thenProp elseProp Obj) (Tt) (Ff) (Empty))
                                                                     
             ]
            )]
         )
       
  )



;; T-CONST but without the need for the other stuff in the judgement
(define (get-type-op1 p)
 (match p
   ['add1 (Fun 'x (Integer) (Integer) (Tt) (Ff) (Empty)) ]
   ['sub1 (Fun 'x (Integer) (Integer)  (Tt) (Ff) (Empty))]
   ['zero? (Fun 'x (Integer) (boolean) (Tt) (Ff) (Empty))]
   ['number? (Fun 'x (any) (boolean) (HasType 'x (Integer)) (NotType 'x (Integer)) (Empty))]
   ['boolean? (Fun 'x (any) (boolean) (HasType 'x (boolean)) (NotType 'x (boolean)) (Empty)) ]
   ['char? (Fun 'x (any) (Character) (HasType 'x (Character)) (NotType 'x (Character)) (Empty)) ]
   ['char->integer (Fun 'x (Character) (Integer) (Tt) (Ff) (Empty)) ]
   ['integer->char (Fun 'x (Integer) (Character) (Tt) (Ff) (Empty)) ]
  )
)

;; T-APP but we know it is a constant function
;; calls the const function to get type, prop is tt | ff, object is empty
(define (get-judge-prim1 f e env f-env)
  ( match (get-type-op1 f)
     ;; gets the type of the primitive
     [ (Fun in-var in-ty out-ty thenProp1 elseProp1 obj1)
       (match (get-judge-e e env f-env)
                         [ (Judge e-ty thenProp2 elseProp2 obj2)

                           
                           (match env

                             ;; if the environment is explosion, it automatically passes he type checker
                             [(TypeEnv expl _)
                              ;; otherwise, whatever operative has to be a sub-type of the the input type of the primitive
                           (if (or expl (is-subtype? in-ty e-ty))

                               ;; does T-APP where it subsitutes the object in for every instance of the constant's parameter i the types, propositions, and object
                               (Judge (subs-type out-ty obj2 in-var ) (subs-prop thenProp1 obj2 in-var) (subs-prop elseProp1 obj2 in-var) (subs-obj obj1 obj2 in-var))

                               ;; throws an error if it failed the type checking
                               (error (string-append "Type Checker: type mismatch\nexpected: " (print-type in-ty) "\ngiven: " (print-type e-ty))))])
                             ]
         )
       ]
     )
  )


;; T-IF
;; Gets judgement information of this if expression
(define (get-judge-if e1 e2 e3 env f-env)
  (match (get-judge-e e1 env f-env)

    ;; Given judgement info of e1
    [(Judge ty1 thenProp1 elseProp1 obj1)

     ;; And judgement info of e2 given the environment has added thenProp1
     ;; And judgement info of e3 given the environementn has added elseProp1
     (match* ((get-judge-e e2 (add-prop thenProp1 env) f-env) (get-judge-e e3 (add-prop elseProp1 env) f-env))
       [((Judge ty2 thenProp2 elseProp2 obj2)(Judge ty3 thenProp3 elseProp3 obj3))
        (let ((ty (get-supertype ty2 ty3)) (obj (get-superobj obj2 obj3)))

          ;; ty2 and ty3 must have the same supertype and obj2 and obj3 must have the same superobj
          ;; follows the rule of T-IF
          (Judge ty (OrProp thenProp2 thenProp3) (OrProp elseProp2 elseProp3) obj)
          
          )]
                    )
                
              ]
             )
)


;; T-APP but it must call a function from the function environment
;; gets the type from the function environemt, prop is tt | ff, object is empty
(define (get-judge-app f es env f-env)

  ;; gets the function type from the function environment
  ( match (lookup-fun f f-env)
     [ (Fun in-var in-ty out-ty thenProp1 elseProp1 obj1)

       ;; gets the judgemnt information for the argument
       (match (get-judge-e (car es) env f-env)
         [ (Judge e-ty thenProp2 elseProp2 obj2)
           (match env

             ;; if the environment is explosion, it automatically passes he type checker            
             [(TypeEnv expl _)

              ;; otherwise, the argument has to be a sub-type of the the input type of the primitive
              (if (or expl (is-subtype? in-ty e-ty))

                  ;; does T-APP where it subsitutes the object in for every instance of the constant's parameter i the types, propositions, and object
                  (Judge (subs-type out-ty obj2 in-var ) (subs-prop thenProp1 obj2 in-var) (subs-prop elseProp1 obj2 in-var) (subs-obj obj1 obj2 in-var))

                  ;; throws an type checker error if the argument fails the type check
                  (error (string-append "Type Checker: type mismatch\nexpected: " (print-type in-ty) "\ngiven: " (print-type e-ty))))]) ]
     )
       ]
  )
 )


