#lang racket
(provide test-runner)
(require rackunit)

;; Commmented tests test errors

(define (test-runner run)
    ;; Alpha examples
  (check-equal? (run 7) 7)
  (check-equal? (run -8) -8)
  (check-equal? (run '(add1 (add1 7))) 9)
   (check-equal? (run '(if #f (sub1 7) 4)) 4)
  ;;(check-equal? (run '(add1 (zero? 7))) 'err)
  (check-equal? (run '(if (sub1 7) 5 4)) 5)
  (check-equal? (run '(if 7 #f #t)) #f)
  (check-equal? (run '(if #t 5 #t)) 5)

  ;; Typed Bumble examples
  
  (check-equal? (run
                 '(begin (define (f (x : Any)) : Boolean (number? x)) (f 5))
                 )
                #t)
  (check-equal? (run
                 '(begin (define (f (x : Any)) : Boolean (number? x)) (f #t))
                 )
                #f)
  
  (check-equal? (run
                 '(begin (define (f (x : Any)) : Boolean (boolean? x)) (f #t))
                 )
                #t)
  
  (check-equal? (run
                 '(begin (define (f (x : Boolean)) : (U Number Boolean) (if x 5 #t)) (f #t))
                 )
                5)
  
  (check-equal? (run
                 '(begin (define (f (x : Boolean)) : Boolean (if x x #t)) (f #f))
                 )
                #t)

   (check-equal? (run
                 ' (begin (define (f (x : Number)) : Number (if (zero? x) 0 (add1 (add1(f (sub1 x)))) )) (f 7))
                 )
                14)

 
  ;    (check-equal? (run
  ;             '(begin (define (f (x : Boolean)) : (U Number Boolean) (if x 5 #t)) (f 5))
  ;)
  ;               'err)
  
  ;       (check-equal? (run
  ;                '(begin (define (f (x : (U Number Boolean))) : Boolean (if x x #t)) (f 5))
  ;)
  ;               'err)
  
  
  ;       (check-equal? (run
  ;                 '(begin (define (f (x : Any)) : (U Boolean Number) (add1 x) )  (f 5)
  ;))
  ;               'err)
  
  
  
  
  
 )