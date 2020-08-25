#lang racket

(provide (except-out (all-from-out racket) #%module-begin #%top-interaction)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         ? U)

(module+ test
  (require rackunit))
(require (for-syntax syntax/parse))
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
    ; [cons #:A [A U] [a A] [l (List A)] (List A)]
    (pattern (c:id typ)
             #:attr def
             #'(define (c) (tt 'c typ)))
    (pattern (c:id (~or [~seq k*:keyword [ki*:id ktyp*:typ]]
                        [p*:id ptyp*:typ])
                   ...
                   typ)
             #:attr def
             #`(define (c {~@ k* [ki* (? ktyp*)]} ...
                          p* ...)
                 (unify (<- ki*) ktyp*) ...
                 (unify ptyp* (<- p*)) ...
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
    [`((~literal ind) . _)
     (error 'ind "bad form ~a" stx)]
    [`((~literal provide) . any) #'(provide . any)]
    [`((~literal require) . any) #'(require . any)]
    [`(f arg* ...)
     #'(displayln (pretty (f arg* ...)))]
    [`x:id #'x]))

(define-syntax-rule (module-begin e* ...)
  (#%module-begin
   (top-interaction e*) ...))

(module reader syntax/module-reader
  inductive)

(module+ test
  (top-interaction
   (ind Bool
        [true Bool]
        [false Bool]))

  (top-interaction
   (ind Nat
        [z Nat]
        [s (n Nat) Nat]))

  (top-interaction
   (ind (List [A U])
        [nil (List (? U))]
        [:: #:A (A U) [a A] [lst (List A)] (List A)]))

  (top-interaction
   (ind (Vec [LEN Nat] [A U])
        [vecnil (Vec (z) (? U))]
        [vec:: #:LEN [LEN Nat] #:A [A U]
               [a A] [v (Vec LEN A)]
               (Vec (s LEN) A)]))

  (define (≡ #:A [A (? U)] a b)
    (: A U)
    (unify (<- a) A)
    (: b A)
    (tt `(≡ ,A ,a ,b) U))
  (define (refl #:A [A (? U)] #:a [a (? A)])
    (tt 'refl (≡ a a)))

  #;(define (vec/length v)
      (define LEN (? Nat))
      (define E (? U))
      (unify (Vec LEN E) (<- v))
      LEN)

  #;(define (sym #:A [A (? U)] #:x [x (? A)] #:y [y (? A)]
                 [P1 (? (≡ x y))])
      (unify (refl) P1)
      (let ([r (refl)])
        (unify (≡ y x) (<- r))
        r))
  #;(pretty (sym))

  #;(define (Nat/+ m n)
      (: m Nat)
      (: n Nat)
      (match (tt-tm (?/get m))
        ['z n]
        [`(s ,m-)
         (s (Nat/+ m- n))]))

  #;(define (+0/Nat #:x [x (? Nat)])
      (let ([r (refl)])
        (unify (≡ (Nat/+ (z) x) x) (<- r))
        r))
  #;(pretty (+0/Nat)))
