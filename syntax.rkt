#lang typed/racket

(struct syntax:inductive
  ([name : String]
   [constructors : (List syntax:constructor)])
  #:transparent)
(struct syntax:constructor
  ([name : String]
   [typ : syntax:type])
  #:transparent)

(define-type syntax:type
  (U syntax:var))
(struct syntax:var ([name : String]) #:transparent)
