#lang racket
(provide env
         env?
         empty-env
         empty-env?
         env-syms
         env-vals
         env-previous
         env-lookup-helper
         env-lookup
         )

; The empty environment is null.
(define empty-env null)
(define empty-env? null?)


(struct env (syms vals previous) #:transparent)
                           

(define (extended-env? e)
(and (list? e)
(not (null? e))
(eq? (first e) 'env)))
                              
  
(define env-a
  (env '(x y)
       '(1 2)
       empty-env))
(define env-b
  (env '(x z)
       '(5 7)
       env-a))
(define env-c
  (env '(x z a b c)
       '(5 7 1 2 3)
       env-a))

(define (env-lookup-helper vars vals symbol)
   (cond [(and (empty? vars)) #f ]
         [(eq? (first vars) symbol) (first vals)]
         [else ( env-lookup-helper ( rest vars) ( rest vals) symbol)]))



(define (env-lookup e sym)
  (cond [(not (symbol? sym)) (error 'env-lookup "sym is not a symbol")]
        [(empty-env? e) (error 'env-lookup "no binding for ~s" sym)]
        )
  (if (equal? (env-lookup-helper (env-syms e) (env-vals e) sym) #f)
      (env-lookup (env-previous e) sym)
      (env-lookup-helper (env-syms e) (env-vals e) sym)
      )
  )
