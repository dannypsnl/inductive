#lang racket

(provide (all-defined-out))

(struct t:ind (name) #:transparent)
(struct t:construction
  (typ name arg*) #:transparent)
(define (: tm typ)
  (unless (t:ind? typ) (error (format "unknown type: ~a" typ)))
  (unless (t:construction? tm) (error "unknown term: ~a" tm))
  (let ([x (t:ind-name (t:construction-typ tm))]
        [t (t:ind-name typ)])
    (unless (eqv? x t)
      (error (format "type mismatched, expected: ~a, but got: ~a" t x))))
  (void))

(define (pretty t)
  (match t
    [(t:construction typ name arg*)
     (let ([name (symbol->string name)])
       (string-join
        (list (if (empty? arg*)
                  name
                  (string-join
                   (cons name (map pretty arg*))
                   " "
                   #:before-first "("
                   #:after-last ")"))
              (pretty typ))
        " : "))]
    [(t:ind n) (symbol->string n)]))
