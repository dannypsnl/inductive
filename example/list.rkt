#lang inductive

(require "prelude.rkt")

(ind (List [A U])
     [nil (List (? U))]
     [:: #:A [A U] [a A] [l (List A)] (List A)])

(nil)
