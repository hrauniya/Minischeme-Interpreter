#lang racket

(require rackunit)
(require "parse.rkt")

(provide parse-tests)

(define parse-tests
  (test-suite
   "Parse tests"
   ;;; lit-exp
   (test-pred "Literal"
              lit-exp?
              (parse 5))
   (test-equal? "get"
                (lit-exp-num (parse 22))
                22)
   ;;; var-exp
   (test-pred "var-exp"
              var-exp?
              (parse 'sym))
   (test-equal? "symbol"              
                (var-exp-symbol (parse 'sym))
                'sym)
   (test-exn "more than 1 input"
             exn:fail?
             (lambda () (parse 'sym 5)))
   ;;; app-exp
   (test-exn "empty"
             exn:fail?
             (lambda () (parse '())))
   (test-equal? "procedure"
                (parse '(foo))
                (app-exp (var-exp 'foo) '()))
   (test-pred "procedure 2"
              var-exp?
              (app-exp-proc (parse '(foo 1 2 3))))
   (test-pred "arguments of app-exp is list"
              list?
              (app-exp-args (parse '(foo 1 2 3))))
   (test-equal? "proc with arg"
                (parse '(foo x y z))
                (app-exp (var-exp 'foo) (list (var-exp 'x) (var-exp 'y) (var-exp 'z))))
   (test-equal? "proc with constant"
                (parse '(barz 1))
                (app-exp (var-exp 'barz) (list (lit-exp 1))))
   ;;; ite-exp
   (test-equal? "con statement with eqv?"
                (parse '(if (eqv? 1 2) 5 6))
                (ite-exp (app-exp (var-exp 'eqv?) (list (lit-exp 1) (lit-exp 2))) (lit-exp 5) (lit-exp 6)))
   (test-equal? "cond statement with symbol"
                (parse '(if (number? y) x 6))
                (ite-exp (app-exp (var-exp 'number?) (list (var-exp 'y))) (var-exp 'x) (lit-exp 6)))
   (test-exn "cond without enough elements"
             exn:fail?
             (lambda () (parse '(if (eqv? 1 2) 5))))
   ;;; let-exp
   (test-equal? "let with numbers"
                (parse '(let ([a 1] [b 5])(+ a b)))
                (let-exp '(a b) (list (lit-exp 1) (lit-exp 5)) (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b)))))
   (test-equal? "let expressions with = symbols"
                (parse '(let ([a x]  [b y])  a))
                (let-exp '(a b) (list (var-exp 'x) (var-exp 'y)) (var-exp 'a)))
   (test-exn "let expressions w/o elements"
             exn:fail?
             (lambda () (parse '(let ([a x]  [b y]) ))))
      ;; Lambda expression parsing tests
   (test-case
    "lambda parsing test 1"
    (check-equal? (parse '(lambda (x) x))
                  (lambda-exp '(x) (var-exp 'x))))
   (test-case
    "lambda parsing test 2"
    (check-equal? (parse '(lambda (x y) (* x y)))
                  (lambda-exp '(x y) (app-exp (var-exp '*) (list (var-exp 'x) (var-exp 'y))))))
      ;;; set-exp
   (test-case
    "set! parsing test 1"
    (check-equal? (parse '(set! x 10))
                  (set-exp 'x (lit-exp 10))))
   (test-case
    "set! parsing test 2"
    (check-equal? (parse '(set! y (+ 1 2)))
                  (set-exp 'y (app-exp (var-exp '+) (list (lit-exp 1) (lit-exp 2))))))

   ;;; begin-exp
   (test-case
    "begin parsing test 1"
    (check-equal? (parse '(begin (set! x 10) (set! y 20) (+ x y)))
                  (begin-exp (list (set-exp 'x (lit-exp 10))
                                   (set-exp 'y (lit-exp 20))
                                   (app-exp (var-exp '+) (list (var-exp 'x) (var-exp 'y)))))))
   (test-case
    "begin parsing test 2"
    (check-equal? (parse '(begin (set! x 1) (set! y (* x 2)) y))
                  (begin-exp (list (set-exp 'x (lit-exp 1))
                                   (set-exp 'y (app-exp (var-exp '*) (list (var-exp 'x) (lit-exp 2))))
                                   (var-exp 'y)))))
   ;;recursion tests parsing tests 
   (test-case
    "parse is let-exp"
    (check-equal? (let-exp? (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 7))))
              #t))

   (test-case
    "the let-exp has let-exp as body"
    (check-equal? (let-exp? (let-exp-proc (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 7)))))
                  #t))

   (test-case
    "the inner let-exp has begin-exp as body"
    (check-equal? (begin-exp? (let-exp-proc (let-exp-proc (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 7))))))
                  #t))
   (test-case
    "the begin-exp expressions have list as body, first element is set-exp"
    (check-equal? (set-exp? (first (begin-exp-exps (let-exp-proc (let-exp-proc (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 7))))))))
                  #t))
    

   ))
