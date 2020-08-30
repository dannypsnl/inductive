#lang racket

(provide (struct-out tt)
         U
         ? ?/get
         unify
         :
         <-)

;;; helper
(define (pretty t)
  (match (?/get t)
    [`(,a* ...) (map pretty a*)]
    [(tt tm ty) `(: ,(pretty tm) ,(pretty ty))]
    [t t]))
(define (?/get p?) (if (parameter? p?) (p?) p?))
;;; core
(struct tt (tm ty)
  #:methods gen:custom-write
  [(define (write-proc tt port mode)
     (fprintf port "~a" (pretty tt)))]
  #:transparent)
; U is greatest type level
(define U 'U)
(define (? ty) (make-parameter (tt (gensym '?) ty)))
(define (occurs v t)
  (match t
    [`(,t* ...)
     (ormap (λ (t) (occurs v t)) t*)]
    [t (equal? v t)]))
(define (unify t1 t2)
  (match* (t1 t2)
    [(_ (? parameter?))
     (unless (or (eqv? t1 (?/get t2)) (not (occurs (?/get t2) t1)))
       (error (format "~a occurs in ~a" (?/get t2) (?/get t1))))
     (t2 (?/get t1))]
    ; swap
    [((? parameter?) _) (unify t2 t1)]
    [(`(,a* ...) `(,b* ...))
     (map unify a* b*)]
    [((tt tm1 ty1) (tt tm2 ty2))
     (unify ty1 ty2)
     (unify tm1 tm2)]
    ; not free variable, then we expect they are the same
    [(_ _) (ty= t1 t2)]))
(define (: term type)
  (unless (ty= (<- (?/get term)) type)
    (error (format "~a is a ~a, not a ~a"
                   (pretty term)
                   (pretty (tt-ty (?/get term)))
                   (pretty type)))))
(define (<- t)
  (match t
    ['U U]
    [t (tt-ty (?/get t))]))
(define (ty= t1 t2)
  (unless (equal? (pretty t1) (pretty t2))
    (error (format "~a != ~a" (pretty t1) (pretty t2)))))
