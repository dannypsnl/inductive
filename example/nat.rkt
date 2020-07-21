#lang inductive

(inductive Nat
           [z Nat]
           [s (-> Nat Nat)])

z
