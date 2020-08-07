#lang inductive

(ind Nat
     [z Nat]
     [s (-> Nat Nat)])

z
(s z)
(s (s z))

Nat

(ind Bool
     [true Bool]
     [false Bool])

true

(ind (List [a Type])
     [nil (List a)]
     [:: (-> a (List a) (List a))])

nil
(:: z nil)
