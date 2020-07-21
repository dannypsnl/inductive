#lang racket

(provide (all-defined-out))

(struct t:ind (name constructor*) #:transparent)
(struct t:construction
  (typ name arg*) #:transparent)
(define (: tm typ)
  (unless (t:ind? typ) (error (format "unknown type: ~a" typ)))
  (unless (t:construction? tm) (error "unknown term: ~a" tm))
  (let ([x (t:construction-typ tm)]
        [t (t:ind-name typ)])
    (unless (eqv? x t)
      (error (format "type mismatched, expected: ~a, but got: ~a" t x))))
  (void))
