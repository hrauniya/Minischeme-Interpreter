#lang racket

(provide parse
         lit-exp
         lit-exp?
         lit-exp-num
         var-exp
         var-exp?
         var-exp-symbol
         app-exp
         app-exp?
         app-exp-proc
         app-exp-args
         ite-exp
         ite-exp?
         ite-exp-cond
         ite-exp-then
         ite-exp-else
         let-exp
         let-exp?
         let-exp-exps
         let-exp-vals
         let-exp-proc
         lambda-exp
         lambda-exp?
         lambda-exp-params
         lambda-exp-body
         set-exp
         set-exp?
         set-exp-sym
         set-exp-exp
         begin-exp
         begin-exp?
         begin-exp-exps
         )

(struct lit-exp (num) #:transparent)
(struct var-exp (symbol) #:transparent)
(struct app-exp (proc args) #:transparent)
(struct ite-exp (cond then else) #:transparent)
(struct let-exp (exps vals proc) #:transparent)
(struct lambda-exp (params body) #:transparent) ; New struct for lambda expressions
(struct set-exp (sym exp) #:transparent)
(struct begin-exp (exps) #:transparent)


(define (parse input)
  (letrec ([parse-error (lambda () (error 'parse "Invalid syntax ~s" input))])
    (cond [(number? input) (lit-exp input)]
          [(symbol? input) (var-exp input)]
          [(list? input)
           (cond [(empty? input) (parse-error)]
                 [(equal? 'if (first input)) (cond [(equal? 4 (length input)) (ite-exp (parse (second input))
                                                                                       (parse (third input))
                                                                                       (parse (fourth input)))]
                                                   [else (parse-error)])]
                 [(equal? 'let (first input)) (cond [(equal? 3 (length input)) (let-exp (map first (second input))
                                                                                        (map parse (map second (second input)))
                                                                                        (parse (third input)))]
                                                    [else (parse-error)])]
                 [(equal? 'lambda (first input)) (cond [(equal? 3 (length input)) (lambda-exp (second input)
                                                                                        (parse (third input)))]
                                                      [else (parse-error)])] ; New case for lambda expressions
                 [(equal? 'set! (first input)) (cond [(equal? 3 (length input)) (set-exp (second input) (parse (third input)))]
                                                    [else (parse-error)])]
                
                 [(equal? 'letrec (first input)) (cond
                                                   [(equal? 3 (length input)) (parse-letrec input)]
                                                   [else (parse-error)])]
                 [(equal? 'begin (first input)) (begin-exp (map parse (rest input)))]
                                            
                 [else (app-exp (parse (first input)) (map parse (rest input)))])]
          [else (parse-error)])))

(define (parse-letrec input)
  (let ([syms (map first (second input))] [new-syms  (map (lambda (s) (gensym)) (map first (second input)))]) 
  (let-exp syms
           (map (lambda (s) (lit-exp 0)) syms)
           (let-exp new-syms
                    (map parse (map second (second input)))
                    (begin-exp (append (map (lambda (s new-s) (set-exp s (parse new-s))) syms new-syms) (list (parse (third input)))))))))
                               
                               
                     
                     
                     
            
  
