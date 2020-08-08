#lang racket

(provide : unify pretty)

(define (occurs v t)
  (match t
    [`(,t* ...)
     (ormap (λ (t) (occurs v t)) t*)]
    (t (equal? v t))))

(define (unify t1 t2)
  (match* (t1 t2)
    [(_ t2) #:when (parameter? t2)
            (unless (or (eqv? t1 (t2)) (not (occurs (t2) t1)))
              (error (format "~a occurs in ~a" (t2) t1)))
            (t2 t1)
            #t]
    [(t1 _) #:when (parameter? t1)
            (unify t2 t1)]
    [(_ t2) #:when (procedure? t2)
            (unify (t2 (build-list (procedure-arity t2)
                                   (λ (x) (cons (cons (make-parameter (gensym '?)) '()) 'Type))))
                   t1)]
    [(t1 _) #:when (procedure? t1)
            (unify t2 t1)]
    [(`(,a . ,a2)
      `(,b . ,b2))
     (and (unify a b)
          (unify a2 b2))]
    [(`(,a* ...) `(,b* ...))
     (andmap unify a* b*)]
    [(_ _)
     (unless (eqv? t1 t2)
       (error (format "cannot unify ~a and ~a" t1 t2)))
     #t]))

(define (: tm typ)
  (match tm
    [tm #:when (pair? tm)
        (let ([x (cdr tm)]
              [name (caar tm)])
          (unless (unify x typ)
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
               (pretty-proc typ)))]
    [t (pretty-proc t)]))
(define (pretty-proc t)
  (cond
    [(parameter? t) (t)]
    [(procedure? t)
     (pretty-proc (t (build-list (procedure-arity t)
                                 (λ (x) (cons (cons (make-parameter (gensym '?)) '()) 'Type)))))]
    [(pair? t)
     (if (empty? (rest (car t)))
         (pretty-proc (caar t))
         (let ([t2 (cadar t)])
           ;;; TODO: unify t2 with memorized type
           `(,(pretty-proc (caar t)) ,(pretty-proc t2))))]
    [else t]))
