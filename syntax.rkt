#lang typed/racket

(struct syntax ([pos : srcloc]) #:transparent)

(struct syntax:inductive syntax
  ([name : String]
   [constructors : (List syntax:constructor)])
  #:transparent)
(struct syntax:constructor syntax
  ([name : String]
   [typ : syntax:type])
  #:transparent)

(define-type syntax:type
  (U syntax:var))
(struct syntax:var syntax
  ([name : String]) #:transparent)
