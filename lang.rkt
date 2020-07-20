#lang nanopass

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
        (e0 e1)
        (inductive v (c* typ*) ...)))

(struct t:ind (name constructor*) #:transparent)
(struct t:construction
  (name arg*) #:transparent)
(define (: tm typ)
  (unless (t:ind? typ) (error "unknown type"))
  (unless (t:construction? tm) (error "unknown term"))
  (ormap (λ (expected-c-name)
           (eqv? (t:construction-name tm)
                 expected-c-name))
         (t:ind-constructor* typ)))

(define (typ->construction name ty)
  (match ty
    [`(-> ,t1 ,t2)
     (λ (x)
       (unless (: x t1) (error (format "type mismatched, expected: ~a, but got: ~a" t1 x)))
       (t:construction name (list x)))]
    [t (t:construction name '())]))

(define-pass expand : (Inductive Expr) (e) -> * ()
  (Expr : Expr (e) -> * ()
        [(inductive ,v
                    (,c* ,typ*) ...)
         (cons `begin (cons
                       `(define ,v (t:ind ,v ,c*))
                       (map (λ (c-name c-typ)
                              `(define c-name (typ->construction c-name c-typ)))
                            c* typ*)))]
        [(,e0 ,e1)
         `(,(eval e0) ,(eval e1))]
        [,v
         `,v]))

(module+ test
  (define-parser ind-parser Inductive)

  (expand (ind-parser `(inductive Nat
                                  [z Nat]
                                  [s (-> Nat Nat)])))
  (expand (ind-parser `z))
  (expand (ind-parser `(s z))))
