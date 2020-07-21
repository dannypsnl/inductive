#lang nanopass

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define variable? symbol?)
(define/match (type? t)
  [(`(-> ,t1 ,t2))
   #:when (and (type? t1) (type? t2))
   #t]
  [(t) #:when (variable? t)
       #t])
(define (constant? x)
  (or (number? x)
      (char? x)
      (string? x)))
;;; quoted expression
(define (datum? x) #t)

(define-language Inductive
  (terminals
   (type (typ))
   (variable (v c)))
  (Expr (e)
        v
        (inductive v (c* typ*) ...)
        (e0 e1 ...)))

(define-parser ind-parser Inductive)

(define (expand e)
  (nanopass-case (Inductive Expr) (ind-parser e)
                 [(inductive ,v
                             (,c* ,typ*) ...)
                  (define (constructor* c* typ*)
                    (match c*
                      [(cons c '()) `,(constructor c (car typ*))]
                      [(cons c c*)
                       `(begin ,(constructor c (car typ*))
                               ,(constructor* c* (cdr typ*)))]))
                  (define (constructor c typ)
                    `(define ,c
                       ,(match typ
                          [`(-> ,t1 ,t2)
                           `(λ (x)
                              (unless (: x ,t1) (error (format "type mismatched, expected: ~a, but got: ~a" ,t1 x)))
                              (t:construction ,c (list x)))]
                          [t `(t:construction ,c '())])))
                  `(begin
                     (define ,v (t:ind ,(symbol->string v) ',c*))
                     ,(constructor* c* typ*))]
                 [(,[e0] ,[e1] ...) `,e]
                 [,v `,v]))

(module+ test
  (expand `(inductive Nat
                      [z Nat]
                      [s (-> Nat Nat)]))
  (expand `z)
  (expand `(s z)))
