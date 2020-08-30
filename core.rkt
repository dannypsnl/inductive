#lang racket

(provide (struct-out tt)
         U
         ? ?/get
         unify
         :
         <-)

(module+ test
  (require rackunit))

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

(module+ test
  (define Bool (tt 'Bool U))
  (define (true) (tt 'true Bool))
  (define (false) (tt 'false Bool))

  (define Nat (tt 'Nat U))
  (define (z) (tt 'z Nat))
  (define (s n)
    (: n Nat)
    (tt `(s ,n) Nat))

  (define (List A)
    (: A U)
    (tt `(List ,A) U))
  (define (nil) (tt 'nil (List (? U))))
  (define (:: #:A [A (? U)] a lst)
    (unify A (<- a))
    (: A U)
    (unify (List A) (<- lst))
    (tt `(:: ,a ,lst) (List A)))

  (define (Vec LEN E)
    (: LEN Nat)
    (: E U)
    (tt `(Vec ,LEN ,E) U))
  (define (vecnil) (tt 'vecnil (Vec (z) (? U))))
  (define (vec:: #:E [E (? U)] #:LEN [LEN (? Nat)] e v)
    (unify E (<- e))
    (: E U)
    (unify (Vec LEN E) (<- v))
    (tt `(vec:: ,e ,v) (Vec (s LEN) E)))

  (define (≡ #:A [A (? U)] a b)
    (: A U)
    (unify (<- a) A)
    (: b A)
    (tt `(≡ ,A ,a ,b) U))
  (define (refl #:A [A (? U)] #:a [a (? A)])
    (tt 'refl (≡ a a)))

  (define (vec/length v)
    (define LEN (? Nat))
    (define E (? U))
    (unify (Vec LEN E) (<- v))
    LEN)

  (define (sym #:A [A (? U)] #:x [x (? A)] #:y [y (? A)]
               [P1 (? (≡ x y))])
    (unify (refl) P1)
    (let ([r (refl)])
      (unify (≡ y x) (<- r))
      r))
  (sym)

  (define (plus m n)
    (: m Nat)
    (: n Nat)
    (match (tt-tm (?/get m))
      ['z n]
      [`(s ,m-)
       (s (plus m- n))]))
  (define (+0/Nat #:x [x (? Nat)])
    (let ([r (refl)])
      (unify (≡ (plus (z) x) x) (<- r))
      r))
  (+0/Nat))
