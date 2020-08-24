#lang inductive

(provide Nat z s)

(ind Nat
     [z Nat]
     [s (n Nat) Nat])
