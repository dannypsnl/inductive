#lang racket

(provide (all-defined-out))

(define (: tm typ)
  (match tm
    [tm #:when (pair? tm)
        (let ([x (cdr tm)]
              [name (caar tm)])
          (unless (eqv? x typ)
            (error (format "type mismatched, expected: ~a, but got: ~a" typ x))))]
    [else (error (format "unknown term: ~a" tm))]))

(define (pretty t)
  (match t
    [`((,name . ,arg*) . ,typ)
     (let ([name (symbol->string name)])
       (format "~a : ~a"
               (if (empty? arg*)
                   name
                   (string-join
                    (cons name (map pretty arg*))
                    " "
                    #:before-first "("
                    #:after-last ")"))
               (pretty-proc typ)))]))
(define (pretty-proc t)
    (if (procedure? t)
        (pretty-proc (t (build-list (procedure-arity t)
                       (λ (x) (cons (cons (gensym '?) '()) 'Type)))))
        (cond
          [(pair? t) (if (empty? (rest (car t)))
                         (caar t)
                         (list (caar t) (pretty-proc (cadar t))))]
          [else t])))
