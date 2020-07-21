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
