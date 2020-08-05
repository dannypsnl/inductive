#lang racket

(provide (all-defined-out))

(struct t:construction
  (typ name arg*) #:transparent)
(define (: tm typ)
  (unless (t:construction? tm) (error "unknown term: ~a" tm))
  (let ([x (t:construction-typ tm)]
        [t typ])
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
    [n (symbol->string n)]))
