#lang inductive

(require "prelude.rkt")

(ind (Vec [A U] [LEN Nat])
     [vecnil (Vec (? U) z)]
     [vec:: #:LEN [LEN Nat]
            [a A] [v (Vec A LEN)]
            (Vec A (s LEN))])

(vecnil)
(vec:: (z) (vecnil))
