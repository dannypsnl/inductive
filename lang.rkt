#lang nanopass

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define variable? symbol?)
(define (constant? x)
  (or (number? x)
      (char? x)
      (string? x)))
;;; quoted expression
(define (datum? x) #t)

(define-language Inductive
  (terminals
   (variable (v c)))
  (Expr (e)
        v
        (inductive v (c* typ*) ...)
        (e0 e1 ...))
  (Type (typ)
        v
        (-> typ* ... typ)))

(define-parser ind-parser Inductive)
