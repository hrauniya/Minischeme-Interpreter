#lang racket

;;Samkeliso and Harsha



(require rackunit rackunit/text-ui rackunit/gui)
(require "env.rkt")

(provide env-tests)
(provide env-lookup-tests)
(define test-env
  (env '(x y)
       '(1 2)
       empty-env))
; Define an environment for testing.
(define env-a
  (env '(x y) '(1 2) empty-env))
(define env-b
  (env '(x z) '(5 7) env-a))
(define env-c
  (env '(x z a b c) '(5 7 1 2 3) env-a))


(define env-tests
  (test-suite
   "Environment tests"
 
   
   ( test-equal? "envioronemnt"
                 (empty-env? env-a) #f)
  
   (test-exn "Empty environment"
                        exn:fail?
                        (lambda () (env-vals empty-env)))


   (test-exn "Empty Environment"
                        exn:fail?
                        (lambda () (env-syms empty-env)))
   ( test-equal? " environment"
                 (env-vals env-a) '(1 2))
   ( test-equal? " environment"
                 (env-vals env-b) '(5 7))
   (test-exn "Empty environment has no previous"
             exn:fail?
             (λ () (env-previous empty-env)))
   ( test-equal? " environmet"
                 (env-previous env-c) env-a)

   ))

   


(define env?-tests
  (test-suite
   "Environment tests"
   (test-true "Normal environment"
              (env? env-a) )
   (test-true "Normal environment"
              (env? env-c) )
   (test-false "Normal environment"
               (env? '(sub (x y) ( 1 2))))
   (test-false "Normal environment"
               (env? '(sub (x y) ( 1 2) empty-env)))
   (test-false "different lenght"
               (env? '(sub  ( 1 2) empty-env)))
   ))


(define env-lookup-tests
  (test-suite
   "Environment tests"
   (test-equal? "normal environment"
                (env-lookup env-a 'x) 1)
   (test-equal? "normal environment"
                (env-lookup env-b 'x) 5)
   (test-equal? "normal environment"
                (env-lookup env-b 'y) 2)
   (test-exn "Empty environment has no previous"
             exn:fail?
             (λ () (env-lookup  empty-env 'y)))
   (test-exn "Empty environment has no previous"
             exn:fail?
             (λ () (env-lookup  env-a 'b)))

   )
  )
