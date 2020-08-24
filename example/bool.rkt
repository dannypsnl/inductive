#lang inductive

(provide Bool true false)

(ind Bool
     [true Bool]
     [false Bool])
