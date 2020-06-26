#lang typed/racket

(struct syntax ([pos : srcloc]) #:transparent)

(define-type syntax:stmt
  (U syntax:stmt:type-of
     syntax:stmt:inductive))
;;; (type-of Nat)
(struct syntax:stmt:type-of syntax
  ([term : syntax:term]) #:transparent)

;;; inductive
; (inductive Nat
;   (z : Nat)
;   (s : Nat -> Nat))
(struct syntax:stmt:inductive syntax
  ([name : String]
   [constructors : (List syntax:constructor)])
  #:transparent)
(struct syntax:constructor syntax
  ([name : String]
   [typ : syntax:type])
  #:transparent)

(define-type syntax:term
  (U syntax:var
     syntax:λ))
(define-type syntax:type
  (U syntax:var))
;;; x
(struct syntax:var syntax
  ([name : String]) #:transparent)
;;; t1 -> t2
(struct syntax:arrow syntax
  ([t1 : syntax:type]
   [t2 : syntax:type]) #:transparent)
(struct syntax:λ syntax
  ([param-name : String]
   [param-typ : syntax:type]
   [body : syntax:term]) #:transparent)
