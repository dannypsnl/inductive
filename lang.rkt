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

(module+ test
  (expand `(inductive Nat
                      [z Nat]
                      [s (-> Nat Nat)]))
  (expand `z)
  (expand `(s z)))
