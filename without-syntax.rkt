#lang racket

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

(define (vec/length v)
  (define LEN (? Nat))
  (define E (? U))
  (unify (Vec LEN E) (<- v))
  LEN)

(define (sym #:A [A (? U)] #:x [x (? A)] #:y [y (? A)]
             [P1 (? (≡ x y))])
  (unify (refl) P1)
  (let ([r (refl)])
    (unify (≡ y x) (<- r))
    r))
(sym)

(define (Nat/+ m n)
  (: m Nat)
  (: n Nat)
  (match (tt-tm (?/get m))
    ['z n]
    [`(s ,m-)
     (s (Nat/+ m- n))]
    [m* (tt `(Nat/+ ,m* ,n) Nat)]))

(define (0+n/Nat #:n [n (? Nat)])
  (let ([r (refl)])
    (unify (≡ (Nat/+ (z) n) n) (<- r))
    r))
(0+n/Nat)

(define (n+0/Nat #:n [n (? Nat)])
  (let ([r (refl)])
    (unify (≡ (Nat/+ n (z)) n) (<- r))
    r))
(n+0/Nat)
