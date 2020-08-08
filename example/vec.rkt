#lang inductive

(ind Nat
     [z Nat]
     [s (-> Nat Nat)])
(ind (Vec [a Type] [len Nat])
     [vecnil (Vec a z)]
     [vec:: (->* ([n Nat]) a (Vec a n) (Vec a (s n)))])

vecnil
(vec:: z vecnil)
