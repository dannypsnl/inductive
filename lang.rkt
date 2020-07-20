#lang nanopass

(module+ test
  (require rackunit))

(define variable? symbol?)
(define/match (type? t)
  [(`(-> ,t1 ,t2))
   #:when (and (type? t1) (type? t2))
   #t]
  [(t) #:when (variable? t)
   #t])

(define-language Inductive
  (terminals
   (type (typ))
   (variable (x c)))
  (Stmt (stmt)
        (inductive x (c* typ*) ...)))

(module+ test
  (define-parser ind-parser Inductive)
  (define prog
    '(inductive Nat
                [z Nat]
                [s (-> Nat Nat)]))
  (ind-parser prog))
