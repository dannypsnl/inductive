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
(define (lookup/type ctx typ)
  (nanopass-case
   (Inductive Type) typ
   [(-> ,typ* ... ,typ)
    (λ (x*)
      (for ([x x*]
            [t typ*])
        (: x (lookup ctx t)))
      (lookup ctx typ))]
   [(,typ ,typ* ...)
    ((lookup ctx typ)
     (map (λ (t) (lookup ctx t)) typ*))]
   [else (lookup ctx typ)]))

(define (constructor c typ v ctx)
  (bind ctx
        c
        (nanopass-case
         (Inductive Type) typ
         [(-> ,typ* ... ,typ)
          (λ (x*)
            (for ([x x*]
                  [t typ*])
              (: x (lookup/type ctx t)))
            (cons (cons c x*) (lookup/type ctx v)))]
         [else (cons (cons c '()) (lookup/type ctx v))])))

(define (eval e ctx)
  (nanopass-case
   (Inductive Expr) (ind-parser e)
   [(inductive ,v (,c* ,typ*) ...)
    (bind ctx v (cons (cons v '()) 'Type))
    (for ([c c*]
          [typ typ*])
      (constructor c typ v ctx))
    #f]
   [(inductive ,v ([,c0* ,typ0*] ...)
               (,c1* ,typ1*) ...)
    (bind ctx v (λ (x*)
                  (for ([x x*]
                        [t typ0*])
                    (: x (lookup/type ctx t)))
                  (cons (cons v x*) 'Type)))
    (for ([c c0*]
          [typ typ0*])
      (bind ctx c (cons (cons (make-parameter c) '()) (lookup/type ctx typ))))
    (for ([c c1*]
          [typ typ1*])
      (constructor c typ v ctx))
    #f]
   [(,[e0] ,[e1] ...)
    (e0 e1)]
   [,v (lookup ctx v)]))

(define-syntax-rule (module-begin EXPR ...)
  (#%module-begin
   (define ctx (context (make-immutable-hash)))
   (bind ctx 'Type 'Type)
   (define all-form (list (eval `EXPR ctx) ...))
   (for-each (λ (form)
               (if form
                   (displayln (pretty form))
                   (void)))
             all-form)))

(define-syntax-rule (top-interaction . exp)
  (begin
    (define ctx (context (make-immutable-hash)))
    (bind ctx 'Type 'Type)
    (eval `exp ctx)))

(module reader syntax/module-reader
  inductive)
