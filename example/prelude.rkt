#lang inductive

(ind Bool
     [true Bool]
     [false Bool])

(ind Nat
     [z Nat]
     [s (n Nat) Nat])
