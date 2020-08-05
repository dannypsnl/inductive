#lang racket

(provide (all-defined-out))

(define (: tm typ)
  (match tm
    [`(the ,x ,name ,arg*)
     (unless (eqv? x typ)
       (error (format "type mismatched, expected: ~a, but got: ~a" typ x)))]
    [else
     (displayln tm)
     (error "unknown term: ~a" tm)]))

(define (pretty t)
  (match t
    [`(the ,typ ,name ,arg*)
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
