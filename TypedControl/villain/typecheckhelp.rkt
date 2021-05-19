#lang racket
(provide (all-defined-out))
(require "ast.rkt")

; HELPER FUNCTIONS TO HELP CHECK SUBSUMPTION RULES

;; returns true if ty2 is a subtype of ty1
(define (is-subtype? ty1 ty2)
  (match ty1

    ;; A type of every type is a subtype of every type
    ;; S-ESUP
    [(Explosion) #t]
    [_ 

     
  (match ty2
    ;; A type of every type is a subtype of every type
    ;; E-SUB
    [(Explosion) #t]

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
     [(Character) (match ty1
                    ;; S-UNIONSUPER
                    [(Union ts1) (is-subtype-union1? ts1 ty2)]
                    ;; S-REFL
                    [(Character) true]
                    [_ false])]
    [_ false]
    )]
    
    )
  )


;; returns true if ty2 is a subtype of any type in ts1
;; S-UNIONSUPER
(define (is-subtype-union1? ts1 ty2)
  (
   match ts1
    ('() false)

    ;; Returns true if there exists an type in the ts1 that ty2 is a subtype of
    ((cons ty1 ts1) (or  (is-subtype? ty1 ty2) (is-subtype-union1? ts1 ty2)))
     
    )
    
)


;; returns true if everything ts2 is a subtype of t1
;; S-UNIONSUB
(define (is-subtype-union2? ty1 ts2)
  (
   match ts2
    ('() true)

    ;; Returns true if all types in ts2 is a subtype of ty1
    ((cons ty2 ts2) (and (is-subtype? ty1 ty2) (is-subtype-union2? ty1 ts2)))
     
    )
    
)

;; HELPER FUNCTIONS TO GET SUPERTYPES AND SUPER OBJECTS FOR T-IF

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

;; gets the subtype of the two types which would have to be the direct subtype of the two
;; If there is no subtype of the two, it returns false
(define (get-subtype ty1 ty2)
 (if (is-subtype? ty1 ty2)
     ty2
     (if (is-subtype? ty2 ty1)
         ty1
         #f
         )
     
     )
)
;; Gets the subtype of the two objects which follow SO-TOP and SO-REFL
;; If there is no common subtype, it returns false
(define (get-superobj obj1 obj2)
  (match* (obj1 obj2)
  [( (Empty) (Empty)) (Empty)]
  [(_ _) (if (eqv? obj1 obj2) obj1 #f)]
  [(_ _) (Empty)]
  
    )
  )

; HELPER FUNCTIONS OF SUBSITUTING for T-APP

;; subsitute each instance of x with obj in the type
(define (subs-type ty obj x)
  (match ty
    [(Explosion) ty]
    [(Integer) ty]
    [(True)    ty]
    [(False)   ty]
    [(Character) ty]
    [(Union ts) (Union (subs-types ts obj x))]
    [(Fun in_var in-ty out-ty if-prop else-prop obj1) (Fun (if (eqv? in_var x) obj in_var)in-ty out-ty (subs-prop if-prop obj x) (subs-prop else-prop x) (subs-obj obj1 obj x))]
   )
 )

;; subsitute each instance of x with obj in the list of types
(define (subs-types ts obj x)
  (match ts
    ['() '()]
    [(cons ty ts) (cons (subs-type ty obj x) (subs-types ts obj x))]
  )
)


;; subsitute each instance of x with obj in the proposition
(define (subs-prop prop obj x)
  (match prop
    [(Tt) prop]
    [(Ff) prop]
    [(HasType x0 ty) (if (eqv? x0 x) (HasType obj ty) prop)]
    [(NotType x0 ty) (if (eqv? x0 x) (NotType obj ty) prop)]
    [(OrProp p1 p2) (OrProp (subs-prop p1 obj x) (subs-prop p2 obj x))]
 
   )
)


;; obj1 replaces x in obj0
(define (subs-obj obj0 obj1 x)
  (match obj0
    [(Empty) (Empty)]
    [_ (if eq? obj0 obj1)
              obj1
              obj0
              ]
  )
)


;; FUNCTIONS FOR LOOKING UP VARIABLES IN THE ENVIRONMENT in T-VAR and T-ABS

;; gets variable's type from the environment
(define (lookup-type x env)

  (match env
    ['() (error "Lookup error")]
    [(TypeEnv #f typeBs) (get-type x typeBs)]
    [_ (Explosion)]
   )
  
)

;; extracts the type from the binding
(define (get-type x bs)
  
  (match bs
    ['() (error "Lookup Error")]
    [(cons (TypeB v ty) bs) (if (eqv? v x)  ty (get-type x bs))]
    )
  )

;; gets the function's type from the function environment
(define (lookup-fun f f-env)

  (match f-env
    ['() (error "Lookup error")]
    [(cons (TypeB v ty) bs) (if (eqv? v f) ty (lookup-fun f bs))]
    )
  
)


         
; HELPERS FOR ADDING PROPOSITIONS

;; Finds if one of the types is Explosion (Every type at once) which is a contrad
(define (find-contra env)
  (match env
    ['()  #f]
    [(cons (TypeB x (Explosion)) ps)  #t]
    [(cons p ps) (find-contra ps)]
   )
 )

; FUNCTIONS FOR HANDLING adding HASTYPE PROPOSITIONS

;; Handles adding a proposotion to a type environment
;; If there is a contradiction in the type environment bindings, explosion = true
(define (add-prop prop env)
  (match env
    [(TypeEnv expl typeBs)  (if expl env
                                (match (simplify-prop prop)

                                  ;; Adding Tt to the environment gives us no new information
                                  [(Tt) env]

                                  ;; Adding Ff to the environment means the entire system explodes due to the contradiction
                                  [(Ff) (TypeEnv #t typeBs)]
                                  ;; Calls the function that handles adding propositon HasType to the type environment
                                  [(HasType x ty) (let ((binds (add-prop-has typeBs x ty)))
                                                    (if (find-contra binds)
                                                        (TypeEnv #t binds)
                                                        (TypeEnv #f binds)))]


                                  ;; Calls the function that handles adding propositon NotType to the type environmen
                                  [(NotType x ty) (let ((binds (add-prop-not typeBs x ty)))
                                                    (if (find-contra binds)
                                                        (TypeEnv #t binds)
                                                        (TypeEnv #f binds)))]

                                  ;; First simplifies the OrProp to short circuit any Tt or Ff
                                  [(OrProp p1 p2)
                                

                                     
                                   

                                      ;; Flattens all the or propositions into one list of hastype propositions and a list of nottype props
                                      (let ((simp (simplify-or (OrProp p1 p2)))) 

                                        ;; Gets binds from adding the simplified or proposition with on list being hastype propositons
                                        ;; and a list of nottype propositions
                                        (let ((binds (add-prop-or simp typeBs)))
                                          (if (find-contra binds)
                                              (TypeEnv #t binds)
                                              (TypeEnv #f binds)))
                                                                              
                                        )
                                      
                                     
                                   ]
                                  
                                  )
                                )
                            
                           ]
    )
)

;; Updates the type environment given the proposition that add-x has ty-x
(define (add-prop-has env add-x ty-x)
   (match env
     ;; If add-x is not already in type environment, we call the helper, where we call
     ;; the helper that add-x is ty-x given we assume add-x could be any type
     ['() (list (TypeB add-x (add-prop-has-help ty-x (any))))]
     [(cons (TypeB x ty) ps) (if (eqv? x add-x)

                                 ;; if we found add-x in the type system, we call the helper
                                 ;; that deduces what type add-x is given it is ty-x given we assumed
                                 ;; add-x was ty
                                 (cons (TypeB add-x  (add-prop-has-help ty-x ty)) ps)

                                 ;; if it was not found, goes through the next type bindings
                                 (add-prop-has ps add-x ty-x)) ]
   )
)

;; the helper gets the common subtype of what we assumed it was from assumed-ty
;; if there is no common subtype, we just say the type is every single type
;; which explodes the system
(define (add-prop-has-help prop-ty assumed-ty)
  (let ((sub-ty (get-subtype assumed-ty prop-ty)))
    (if sub-ty
        sub-ty
        (Explosion)
        )
    )
  )

; FUNCTIONS FOR HANDLING adding NOTTYPE PROPOSITIONS

;; Updates the type environment given the proposition that add-x is NOT ty-x
(define (add-prop-not env add-x ty-x)
  (match env

    ;; If add-x is not already in type environment, we call the helper, where we call
     ;; the helper that add-x is NOT ty-x given we assume add-x could be any type
    ['() (list (TypeB add-x (add-prop-has-help ty-x (any))))]
    [(cons (TypeB x ty) ps) (if (eqv? x add-x)

                                ;; if we found add-x in the type system, we call the helper
                                ;; that deduces what type add-x is given it is NOT ty-x given we assumed
                                ;; add-x was ty
                                (cons (TypeB add-x (add-prop-not-help ty-x ty)) ps)
                                (add-prop-has ps add-x ty-x))
                            ]
    )
  )


(define (add-prop-not-help add-ty assumed-ty)

  ;; if we say that some is NOT add-ty but we know it is type assumed-ty
  ;; and assumed-ty is a subtype of add-ty, that is a contradiction and leads to
  ;; explosion
  (if (is-subtype? add-ty assumed-ty)
      (Explosion)

      ;; If it is not subtype, then call the helper that determines the type given something is
      ;; assumed-ty but not add-ty
     (rem add-ty assumed-ty)
   )
 )


;; A helper function that determines the type given something is assumed-ty but not not-ty
(define (rem not-ty assumed-ty)
  (match* (not-ty assumed-ty)

    ;; If there are multiple types the type could not be, removed all of them
    [((Union to-rems) _) (rem-xs-from to-rems assumed-ty)]
    [((Integer) (Integer)) (Explosion)]
    [((True) (True))    (Explosion)]
    [((False) (False))   (Explosion)]
    [((Character) (Character)) (Explosion)]

    ;; If there is a single type but we know it is a union of multiple types, we remove that type from the union
    [(_ (Union ts)) (match (rem-from-xs not-ty ts)

                      ;; while this should never happen based on he curren type system, for future extenisions,
                      ;; a union of no type is a contradiction which leads to explosion
                      ['() (Explosion)]
                      [list-ty (Union list-ty)]
                      )]
    [(_ _) assumed-ty]
  )
)

;; removed every type from the list of not types in assumed-ty
(define (rem-xs-from not-types assumed-ty)
  (match assumed-ty

    ;; If assumed-ty is Explosion, there is nothing more to prove
    [(Explosion) (Explosion)]
    [_
  (match not-types
    [(list x) (rem x assumed-ty)]
    [(cons x xs) (rem-xs-from xs (rem x assumed-ty))]
    )]           
    )
  )


;; removes the one type from the list of types
(define (rem-from-xs not-type assumed-types)
  (match assumed-types

    ;; There is nothing left to remove
    ['() '()]
    [(cons t ts) (match (rem not-type t)

                   ;; we removed the one type from the union since it cannot be this type
                   [(Explosion) (rem-from-xs not-type ts)]
                   [new-t   (cons t (rem-from-xs not-type ts))]
                   )              
                 ]
    )
  )

; FUNCTIONS FOR HANDLING adding ORTYPE PROPOSITIONS

;; Adds the proposition of ors
(define (add-prop-or prop env)
  (match prop
    [(OrPropSimp x has not)

     ;; An OrProp can be simplified to a Union of possible types
     ;; where the list is all the types from the HasType proposiotns
     ;; and all the (Any - type) from the NoType propositions
     (let ((ty (simp-union (Union (append (get-not not) has)))))
                              (add-prop-has env x ty)
                              
                              )
                            ]
    )
  )

;; Simplifies the or proposition by short circuiting
(define (simplify-prop prop)
  (match prop
    [(OrProp p1 p2)

     (let ((simp1 (simplify-prop p1)) (simp2 (simplify-prop p2)))
                                       (match simp1
                                         [(Tt) (Tt)]
                                         [(Ff) simp2]
                                         [_ (match simp2
                                              [(Tt) (Tt)]
                                              [(Ff) simp1]
                                              [(Tt) (OrProp simp1 simp2) ]
                                              )]
                                         ))]
    [_ prop]
    
   
  )
)

;; Puts all the types in hasType propositions in one list and all types in notType propositions the other list
(define (simplify-or prop)
  (match prop
    [(OrProp p1 p2)

     (let ((simp1 (simplify-or p1)) (simp2 (simplify-or p2)))
                                       (match* (simp1 simp2)
                                         [((OrPropSimp x1 has1 not1 ) (OrPropSimp x2 has2 not2)) (OrPropSimp x1 (append has1 has2) (append not1 not2))]
                    
                      )
                                         )]
    [(HasType x ty) (OrPropSimp x (list ty) '())]
    [(NotType x ty) (OrPropSimp x '() (list ty))]
    )
 )


;; Gets the type of a type that cannot be any type in the type list
;; which is just Any without the types of the list
(define (get-not not-ty-lst)
  (match not-ty-lst
    ['() '()]
    [(cons t ts) (cons (add-prop-not-help t (any)) (get-not ts) )]
    )
  )


; FUNCTIONS FOR PRINTING THE TYPE FOR THE TYPECHECKER ERRORS

;; Used for type checking errors. Prints type
;; Has some non-complete matchings for types like Boolean and Any
(define (print-type ty)
  (match ty
    
    [(Integer) "Number"]
    [(True)    "True"]
    [(False)   "False"]
    [(Character) "Char"]
    [(Union (list (Integer) (True) (False) (Character))) "Any"]
    [(Union (list (True) (False))) "Boolean"]
    [(Union (list (False) (True))) "Boolean"]
    [(Union ts) (string-append "(U " (print-types ts) ")")]
    [_ ty]
   )
 )

;; Prints all the types in the list of union
(define (print-types ts)
  (match ts
    [(list ty) (print-type ty)]
    [(cons ty ts) (string-append (print-type ty) " " (print-types ts))]
   )
)

