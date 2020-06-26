#lang typed/racket

(provide (all-defined-out))

(struct syn ([pos : srcloc]) #:transparent)

(define-type syn:stmt
  (U syn:stmt:type-of
     syn:stmt:inductive))
; (type-of z)
; => Nat
(struct syn:stmt:type-of syn
  ([term : syn:term]) #:transparent)

;;; inductive
; (inductive Nat
;   (z : Nat)
;   (s : Nat -> Nat))
(struct syn:stmt:inductive syn
  ([name : String]
   [constructor* : (Listof syn:constructor)])
  #:transparent)
(struct syn:constructor syn
  ([name : String]
   [typ : syn:type])
  #:transparent)

(define-type syn:term
  (U syn:var
     syn:λ))
(define-type syn:type
  (U syn:var
     syn:arrow))
;;; x
(struct syn:var syn
  ([name : String]) #:transparent)
;;; t1 -> t2
(struct syn:arrow syn
  ([t1 : syn:type]
   [t2 : syn:type]) #:transparent)
(struct syn:λ syn
  ([param-name : String]
   [param-typ : syn:type]
   [body : syn:term]) #:transparent)
