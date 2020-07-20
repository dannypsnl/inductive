#lang nanopass

(require "core.rkt")
(module+ test
  (require rackunit))

(define variable? symbol?)
(define/match (type? t)
  [(`(-> ,t1 ,t2))
   #:when (and (type? t1) (type? t2))
   #t]
  [(t) #:when (variable? t)
       #t])

(define-language Inductive
  (terminals
   (type (typ))
   (variable (v c tv)))
  (Expr (e)
        v
        (e0 e1* ...)
        (inductive v (c* typ*) ...)))

(define-pass expand : (Inductive Expr) (e) -> * ()
  (Expr : Expr (e) -> * ()
        [(inductive ,v
                    (,c* ,typ*) ...)
         (cons `begin (cons
                       `(define ,v (t:ind ,v ,c*))
                       (map (λ (c-name c-typ)
                              `(define c-name (typ->construction c-name c-typ)))
                            c* typ*)))]
        [(,e0 ,e1* ...)
         `(,(eval e0) ,(map eval e1*))]
        [,v
         `,v]))

(module+ test
  (define-parser ind-parser Inductive)

  (expand (ind-parser `(inductive Nat
                                  [z Nat]
                                  [s (-> Nat Nat)])))
  (expand (ind-parser `z))
  (expand (ind-parser `(s z))))
