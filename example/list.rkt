#lang inductive

(ind Nat
     [z Nat]
     [s (-> Nat Nat)])
(ind (List [a Type])
     [nil (List a)]
     [:: (-> a (List a) (List a))])

List
nil
(:: z nil)
(:: (s (s z)) nil)
