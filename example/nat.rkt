#lang inductive

(inductive Nat
           [z Nat]
           [s (-> Nat Nat)])

z
(s z)
(s (s z))

(inductive Bool
           [true Bool]
           [false Bool])

