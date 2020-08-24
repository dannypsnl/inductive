#lang inductive

(require "nat.rkt")

(ind (List [A U])
     [nil (List (? U))]
     [:: [A (? U)] [a A] [l (List A)] (List A)])
