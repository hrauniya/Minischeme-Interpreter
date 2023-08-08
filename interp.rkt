#lang racket

(require "env.rkt" "parse.rkt")
(provide eval-exp
         init-env
         prim-env)



(struct prim-proc (op) #:transparent)

(struct closure (params body env) #:transparent)

(define primitive-operators '(+ - * /
                                add1 sub1 negate
                                list cons car cdr
                                eqv? lt? gt? leq? geq?
                                null? list? number?))





(define boolean-env
  (env '(False True) (map box '(False True)) empty-env))



(define prim-env
  (env primitive-operators
       (map box (map prim-proc primitive-operators))
       boolean-env))




(define init-env
  (env '(x y null)
       (map box '(23 42 ()))
       prim-env))


(define (apply-primitive-op op args)
  (letrec ([parse-error (lambda (n) (error 'apply-primitive-op "The expected number of arguments does not match the given number.\n Expected ~s.\n Given ~s." n (length args)))]
           [contract-violation (lambda (ex gv) (error 'apply-primitive-op "Contract violation.\n Expected ~s.\n Given ~s" ex gv))])
    (cond [(eq? op '+) (apply + args)]
          [(eq? op '-) (apply - args)]
          [(eq? op '*) (apply * args)]
          [(eq? op '/) (apply / args)]
          [(eq? op 'add1) (cond [(equal? 1 (length args)) (cond [(number? (first args)) (+ (first args) 1)]
                                                                [else (contract-violation 'number? (first args))])]
                                [else (parse-error 1)])]
          [(eq? op 'sub1) (cond [(equal? 1 (length args)) (cond [(number? (first args)) (- (first args) 1)]
                                                                [else (contract-violation 'number? (first args))])]
                                [else (parse-error 1)])]
          [(eq? op 'negate) (cond [(equal? 1 (length args)) (cond [(number? (first args)) (- 0 (first args))]
                                                                  [else (contract-violation 'number? (first args))])]
                                  [else (parse-error 1)])]
          [(eq? op 'list) (apply list args)]
          
          ;;; pair/list operations
          [(eq? op 'cdr) (cond [(equal? 1 (length args)) (cond [(or (pair? args) (and (list? (first args)) (> (length (first args)) 0))) (apply cdr args)]
                                                               [else (contract-violation 'pair? (first args))])]
                               [else (parse-error 1)])]
          [(eq? op 'car) (cond [(equal? 1 (length args)) (cond [(or (pair? args) (and (list? (first args)) (> (length (first args)) 0))) (apply car args)]
                                                               [else (contract-violation 'pair? (first args))])]
                               [else (parse-error 1)])]
          [(eq? op 'cons) (cond [(equal? 2 (length args)) (apply cons args)]
                                [else (parse-error 2)])]

          ;;; pred
          [(eq? op 'eqv?) (cond [(equal? 2 (length args))
                                 (cond [(equal? (first args) (second args)) 'True]
                                       [else (eval-exp (parse 'False) init-env)])]
                                [else (parse-error 2)])]
          [(eq? op 'lt?) (cond [(apply < args) (eval-exp (parse 'True) init-env)]
                               [else (eval-exp (parse 'False) init-env)])]
          [(eq? op 'gt?) (cond [(apply > args) (eval-exp (parse 'True) init-env)]
                               [else (eval-exp (parse 'False) init-env)])]
          [(eq? op 'leq?) (cond [(apply <= args) (eval-exp (parse 'True) init-env)]
                                [else (eval-exp (parse 'False) init-env)])]
          [(eq? op 'geq?) (cond [(apply >= args) (eval-exp (parse 'True) init-env)]
                                [else (eval-exp (parse 'False) init-env)])]
          ;;; primitive prod
          [(eq? op 'null?) (cond [(equal? 1 (length args)) (cond [(apply null? args) (eval-exp (parse 'True) init-env)]
                                                                 [else (eval-exp (parse 'False) init-env)])]
                                 [else (parse-error 1)])]
          [(eq? op 'list?) (cond [(equal? 1 (length args)) (cond [(apply list? args) (eval-exp (parse 'True) init-env)]
                                                                 [else (eval-exp (parse 'False) init-env)])]
                                 [else (parse-error 1)])]
          [(eq? op 'number?) (cond [(equal? 1 (length args)) (cond [(apply number? args) (eval-exp (parse 'True) init-env)]
                                                                   [else (eval-exp (parse 'False) init-env)])]
                                   [else (parse-error 1)])]
          [else (error 'apply-primitive-op "Unknown primitive: ~s" op)])))


(define (apply-proc proc args)
  (cond [(prim-proc? proc)
         (apply-primitive-op (prim-proc-op proc) args)]
        [(closure? proc) ; New case for closures
         (eval-exp (closure-body proc)
                   (env (closure-params proc)
                        (map box args)
                        (closure-env proc)))]
        [else (error 'apply-proc "bad procedure: ~s" proc)]))


(define (eval-exp tree e)
  (cond [(lit-exp? tree) (lit-exp-num tree)]
        [(var-exp? tree) (unbox (env-lookup e (var-exp-symbol tree)))]
        [(app-exp? tree)
         (apply-proc (eval-exp (app-exp-proc tree) e) (map (lambda (arg) (eval-exp arg e)) (app-exp-args tree)))]
        [(ite-exp? tree)
         (let ([pred (eval-exp (ite-exp-cond tree) e)]) (cond [(or (equal? 'False pred) (if (number? pred)
                                                                                            (zero? pred)
                                                                                            #f)) (eval-exp (ite-exp-else tree) e)]
                                                              [else (eval-exp (ite-exp-then tree) e)]))]
        [(let-exp? tree)
         (eval-exp (let-exp-proc tree) (env (let-exp-exps tree)
                                             (map box (map (lambda (t) (eval-exp t e)) (let-exp-vals tree)))
                                            e))]
        [(lambda-exp? tree) (closure (lambda-exp-params tree) (lambda-exp-body tree) e)] ; New case for lambda expressions
        [(set-exp? tree) (set-box! (env-lookup e (set-exp-sym tree)) (eval-exp (set-exp-exp tree) e))] ; New case for set-exp
        [(begin-exp? tree) (foldl (lambda (exp acc) (eval-exp exp e)) (void) (begin-exp-exps tree))]
        [else (error 'eval-exp "Invalid tree: ~s" tree)]))
