# inductive

Try to build up inductive construction first then add limitation.

### Current Status

```racket
#lang inductive

(provide Bool true false
         Nat z s
         List nil ::)

(ind Bool
     [true Bool]
     [false Bool])

(ind Nat
     [z Nat]
     [s (n Nat) Nat])

(ind (List [A U])
     [nil (List (? U))]
     [:: #:A [A U] [a A] [l (List A)] (List A)])
```
