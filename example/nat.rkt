#lang inductive

(inductive Nat
           [z Nat]
           [s (-> Nat Nat)])

z
(s z)
(s (s z))

Nat

(inductive Bool
           [true Bool]
           [false Bool])

true

(inductive List ([a Type])
           [nil (List a)]
           [:: (-> a (List a) (List a))])

nil
(:: z nil)
