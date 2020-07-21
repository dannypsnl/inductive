#lang nanopass

(require "core.rkt")
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
        (e e* ...)))

(define-language Racket
  (terminals
   (variable (v))
   (datum (d))
   (constant (constant)))
  (Expr (e body)
        v
        'd
        constant
        ;;; forms
        ; define/set!
        (define v e)
        (set! v e)
        ; lambda/λ
        (λ (v* ...) body* ... body)
        ; begin
        (begin body* ... body)
        ; application
        (e e* ...)))

(define-pass expand : Inductive (e) -> Racket ()
  (definitions)
  (Expr : Expr (e) -> Expr ()
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
                     (t:construction ',c (list x)))]
                 [t `(t:construction ',c '())])))
         `(begin
            (define ,v (t:ind ',v ',c*))
            ,(constructor* c* typ*) ...)])
  (Expr e))

(module+ test
  (define-parser ind-parser Inductive)

  (expand (ind-parser `(inductive Nat
                                  [z Nat]
                                  [s (-> Nat Nat)])))
  (expand (ind-parser `z))
  (expand (ind-parser `(s z))))
