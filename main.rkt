#lang racket

(provide (except-out (all-from-out racket) #%module-begin #%top-interaction)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))

(require (for-syntax syntax/parse)
         racket/syntax
         syntax/stx)
(require nanopass)
(require "lang.rkt"
         "core.rkt")

(struct context (map)
  #:mutable
  #:transparent)
(define (bind ctx v x)
  (unless (not (hash-ref (context-map ctx) v (λ () #f)))
    (error (format "redefined: ~a" v)))
  (set-context-map! ctx
                    (hash-set (context-map ctx) v x)))
(define (lookup ctx v)
  (hash-ref (context-map ctx) v))

(define (eval e ctx)
  (nanopass-case (Inductive Expr) (ind-parser e)
                 [(inductive ,v (,c* ,typ*) ...)
                  (define (constructor c typ)
                    (bind ctx
                          c
                          (match typ
                            [`(-> ,t1 ,t2)
                             (λ (x)
                               (: x (lookup ctx t1))
                               (t:construction (lookup ctx v) c (list x)))]
                            [t (t:construction (lookup ctx v) c '())])))
                  (bind ctx v (t:ind v))
                  (for-each constructor
                            c*
                            typ*)
                  #f]
                 [(,[e0] ,[e1] ...)
                  (apply e0 e1)]
                 [,v (lookup ctx v)]))

(define-syntax-rule (module-begin EXPR ...)
  (#%module-begin
   (define ctx (context (make-immutable-hash)))
   (define all-form (list (eval `EXPR ctx) ...))
   (for-each (λ (form)
               (if form
                   (displayln (pretty form))
                   (void)))
             all-form)))

(define-syntax-rule (top-interaction . exp)
  (begin
    (define ctx (context (make-immutable-hash)))
    (eval `exp ctx)))

(module reader syntax/module-reader
  inductive)
