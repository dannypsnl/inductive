#lang racket

#|
FIXME: add syntax wrapper to fix example
NOTE
* pretty every top level application
* ind provide new inductive data type
* define-for-syntax?
|#
(module+ test
  (require rackunit))

(require "core.rkt")

(define Bool (tt 'Bool U))
(define (true) (tt 'true Bool))
(define (false) (tt 'false Bool))

(define Nat (tt 'Nat U))
(define (z) (tt 'z Nat))
(define (s n)
  (: n Nat)
  (tt `(s ,n) Nat))

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

(module+ test
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

  (define (sym #:A [A (? U)] #:x [x (? A)] #:y [y (? A)]
               [P1 (? (≡ x y))])
    (unify (refl) P1)
    (let ([r (refl)])
      (unify (≡ y x) (<- r))
      r))
  (pretty (sym))

  (define (plus m n)
    (: m Nat)
    (: n Nat)
    (match (tt-tm (?/get m))
      ['z n]
      [`(s ,m-)
       (s (plus m- n))]))
  (check-equal? (pretty (plus (s (z)) (s (z))))
                '(: (s (: (s (: z (: Nat U))) (: Nat U))) (: Nat U)))

  (define (+0/Nat #:x [x (? Nat)])
    (let ([r (refl)])
      (unify (≡ (plus (z) x) x) (<- r))
      r))
  (pretty (+0/Nat)))
