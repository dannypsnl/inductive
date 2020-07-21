#lang racket

(provide (except-out (all-from-out racket) #%module-begin #%top-interaction)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))

(require (for-syntax syntax/parse)
         racket/syntax
         syntax/stx)
(require "lang.rkt"
         "core.rkt")

(define (eval e)
  (match e
    [`(begin ,b* ... ,b)
     (map eval b*)
     (eval b)]
    [`(define ,v ,e)
     (define v (eval e))
     #f]
    [`(λ (,x* ...) ,b* ... ,b)
     (λ (x*)
       (map eval b*)
       (eval b))]
    [`'(,e* ...) e]
    [`(',f ,arg* ...)
     (apply (eval f) (map eval arg*))]
    [`,c c]))

(define-syntax-rule (module-begin EXPR ...)
  (#%module-begin
   (define all-form (list (expand `EXPR) ...))
   (for-each (λ (form)
               (let ([result (eval form)])
                 (if result
                     (displayln result)
                     (void))))
             all-form)))

(define-syntax-rule (top-interaction . exp)
  (eval (expand `exp)))

(module reader syntax/module-reader
  inductive)
