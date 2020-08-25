#lang inductive

(provide Bool true false
         Nat z s
         List nil ::
         Vec vecnil vec::)

(ind Bool
     [true Bool]
     [false Bool])

(ind Nat
     [z Nat]
     [s (n Nat) Nat])

(ind (List [A U])
     [nil (List (? U))]
     [:: #:A [A U] [a A] [l (List A)] (List A)])

(ind (Vec [A U] [LEN Nat])
     [vecnil (Vec (? U) z)]
     [vec:: #:LEN [LEN Nat] #:A [A U]
            [a A] [v (Vec A LEN)]
            (Vec A (s LEN))])
