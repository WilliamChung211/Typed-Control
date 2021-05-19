#lang racket
(provide test-runner)
(require rackunit)

(define (test-runner run)
  ;; Alpha examples
  (check-equal? (run 7) 7)
  (check-equal? (run -8) -8)
  (check-equal? (run '(add1 (add1 7))) 9)
   (check-equal? (run '(if #f (sub1 7) 4)) 4)
  ;;(check-equal? (run '(add1 (zero? 7))) 'err)
 ; (check-equal? (run '(if (sub1 7) 5 4)) 'err)
  ;;(check-equal? (run '(if 7 #f #t)) 'err)
  ;;(check-equal? (run '(if #t 5 #t)) 'err)


  
 )