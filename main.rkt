#lang racket

(provide (except-out (all-from-out racket) #%module-begin #%top-interaction)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))
#|
FIXME: add syntax wrapper to fix example
NOTE
* pretty every top level application
* ind provide new inductive data type
* provide match syntax wrapper
* define-for-syntax?
|#
(module+ test
  (require rackunit))
(require (for-syntax syntax/parse)
         racket/syntax
         syntax/stx)
(require "core.rkt")

(define-syntax (top-interaction stx)
  (define-syntax-class typ
    (pattern t:id)
    (pattern (t:id tp*:typ ...)))
  (define-syntax-class constructor
    ; [z Nat]
    ; [s (n Nat) Nat]
    ; or
    ; [nil (List (? U))]
    ; [cons [#:A (? U)] [a A] [l (List A)] (List A)]
    (pattern (c:id [p*:id ptyp*:typ] ... typ)
             #:attr def
             #'(define (c p* ...)
                 (: p* ptyp*) ...
                 (tt (list 'c p* ...) typ))))
  (syntax-parse stx
    [`((~literal ind) name:id c*:constructor ...)
     #'(begin
         (define name (tt 'name U))
         c*.def ...)]
    [`((~literal ind) (name:id [tp*:id tptyp*:typ] ...) c*:constructor ...)
     #'(begin
         (define (name tp* ...)
           (: tp* tptyp*) ...
           (tt (list 'name tp* ...) U))
         c*.def ...)]
    [`((~literal ind) . bad)
     (error 'ind "bad form ~a" #'(ind bad))]
    [`((~literal provide) id* ...) #'(provide id* ...)]
    [`((~literal require) id* ...) #'(require id* ...)]
    [`(f arg* ...)
     #'(pretty (f arg* ...))]
    [`x:id #'x]))

(define-syntax-rule (module-begin e* ...)
  (#%module-begin
   (top-interaction e*) ...))

(module reader syntax/module-reader
  inductive)

#;(module+ test
    (define (List A)
      (: A U)
      (tt `(List ,A) U))
    (define (nil) (tt 'nil (List (? U))))
    (define (:: #:A [A (? U)] a lst)
      (unify A (<- a))
      (: A U)
      (unify (List A) (<- lst))
      (tt `(:: ,a ,lst) (List A)))

    (define (Vec LEN E)
      (: LEN Nat)
      (: E U)
      (tt `(Vec ,LEN ,E) U))
    (define (vecnil) (tt 'vecnil (Vec (z) (? U))))
    (define (vec:: #:E [E (? U)] #:LEN [LEN (? Nat)] e v)
      (unify E (<- e))
      (: E U)
      (unify (Vec LEN E) (<- v))
      (tt `(vec:: ,e ,v) (Vec (s LEN) E)))

    (define (≡ #:A [A (? U)] a b)
      (: A U)
      (unify (<- a) A)
      (: b A)
      (tt `(≡ ,A ,a ,b) U))
    (define (refl #:A [A (? U)] #:a [a (? A)])
      (tt 'refl (≡ a a)))

    (check-equal? (pretty (s (s (z))))
                  '(: (s (: (s (: z (: Nat U))) (: Nat U))) (: Nat U)))
    (check-equal? (pretty (List Nat))
                  '(: (List (: Nat U)) U))
    (check-equal? (pretty (:: (s (z)) (:: (z) (nil))))
                  '(:
                    (::
                     (: (s (: z (: Nat U))) (: Nat U))
                     (:
                      (:: (: z (: Nat U)) (: nil (: (List (: Nat U)) U)))
                      (: (List (: Nat U)) U)))
                    (: (List (: Nat U)) U)))
    (check-equal? (pretty (vec:: (z) (vecnil)))
                  '(:
                    (vec:: (: z (: Nat U)) (: vecnil (: (Vec (: z (: Nat U)) (: Nat U)) U)))
                    (: (Vec (: (s (: z (: Nat U))) (: Nat U)) (: Nat U)) U)))

    (define (vec/length v)
      (define LEN (? Nat))
      (define E (? U))
      (unify (Vec LEN E) (<- v))
      LEN)
    (check-equal? (pretty (vec/length (vec:: (z) (vec:: (z) (vecnil)))))
                  '(: (s (: (s (: z (: Nat U))) (: Nat U))) (: Nat U)))

    #;(define (sym #:A [A (? U)] #:x [x (? A)] #:y [y (? A)]
                   [P1 (? (≡ x y))])
        (unify (refl) P1)
        (let ([r (refl)])
          (unify (≡ y x) (<- r))
          r))
    #;(pretty (sym))

    (define (Nat/+ m n)
      (: m Nat)
      (: n Nat)
      (match (tt-tm (?/get m))
        ['z n]
        [`(s ,m-)
         (s (Nat/+ m- n))]))
    (check-equal? (pretty (Nat/+ (s (z)) (s (z))))
                  '(: (s (: (s (: z (: Nat U))) (: Nat U))) (: Nat U)))

    #;(define (+0/Nat #:x [x (? Nat)])
        (let ([r (refl)])
          (unify (≡ (Nat/+ (z) x) x) (<- r))
          r))
    #;(pretty (+0/Nat)))
