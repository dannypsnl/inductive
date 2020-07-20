#lang racket

(provide (all-defined-out))

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
