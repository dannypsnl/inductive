#lang inductive

(provide Bool true false
         Nat z s)

(ind Bool
     [true Bool]
     [false Bool])

(ind Nat
     [z Nat]
     [s (n Nat) Nat])
