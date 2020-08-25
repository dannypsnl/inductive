#lang inductive

(provide Bool true false
         Nat z s
         List nil ::
         Vec vecnil vec::
         Vec/length
         ≡ refl)

(ind Bool
     [true Bool]
     [false Bool])

(ind Nat
     [z Nat]
     [s (n Nat) Nat])

(ind (List [A U])
     [nil (List (? U))]
     [:: #:A [A U] [a A] [l (List A)] (List A)])

(ind (Vec [LEN Nat] [A U])
     [vecnil (Vec (z) (? U))]
     [vec:: #:LEN [LEN Nat] #:A [A U]
            [a A] [v (Vec LEN A)]
            (Vec (s LEN) A)])
(define (Vec/length #:LEN [LEN Nat] #:A [A U] [v (Vec LEN A)])
  LEN)

(ind (≡ #:A [A U] [a A] [b A])
     [refl #:A [A U] #:a [a A] (≡ a a)])
