#lang inductive

(require "prelude.rkt")

(define (sym #:A [A U] #:x [x A] #:y [y A]
             #:P1 [P1 (≡ x y)]
             ;----------------
             -> (≡ y x))
  (refl))
(sym)

(define (trans #:A [A U] #:x [x A] #:y [y A] #:z [z A]
             #:P1 [P1 (≡ x y)]
             #:P2 [P2 (≡ y z)]
             ;----------------
             -> (≡ x z))
  (refl))
(trans)
