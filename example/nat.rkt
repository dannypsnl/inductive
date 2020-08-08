#lang inductive

(ind Nat
     [z Nat]
     [s (-> Nat Nat)])

Nat
z
(s z)
(s (s z))
