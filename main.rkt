#lang racket

(provide (except-out (all-from-out racket) #%module-begin #%top-interaction)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))

(require (for-syntax syntax/parse)
         racket/syntax
         syntax/stx)
(require "lang.rkt")

(define (eval e)
  ;(displayln (ind-parser e))
  (expand
   (ind-parser e)))

(define-syntax (run stx)
  (syntax-parse stx
    [`((~literal inductive) v (c* typ*) ...)
     #'(eval '(inductive v (c* typ*) ...))]
    [`(e e* ...)
     #'(eval '(e e* ...))]
    [`v:id #'(eval 'v)]))

(define-syntax-rule (module-begin EXPR ...)
  (#%module-begin
   (define all-form (list (run EXPR) ...))
   (for-each (λ (form)
               (displayln form))
             all-form)))

(define-syntax-rule (top-interaction . exp)
  (run exp))

(module reader syntax/module-reader
  inductive)
