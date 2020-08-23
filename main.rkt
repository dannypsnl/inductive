#lang racket

;;; FIXME: add syntax wrapper to fix example
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
(struct tt (tm ty) #:transparent)
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
  (unless (ty= (tt-ty (?/get term)) type)
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
  (: (?/get A) U)
  (unify (List (?/get A)) (<- lst))
  ; NOTE: we can use pretty is because A should be determined
  (tt `(:: ,a ,lst) (pretty (List A))))

(define (Vec LEN E)
  (: LEN Nat)
  (: E U)
  (tt `(Vec ,LEN ,E) U))
(define (vecnil) (tt 'vecnil (Vec (z) (? U))))
(define (vec:: #:E [E (? U)] #:LEN [LEN (? Nat)] e v)
  (unify E (<- e))
  (: (?/get E) U)
  (unify (Vec LEN (?/get E)) (<- v))
  (tt `(vec:: ,e ,v) (Vec (s LEN) E)))

(define (vec/length v)
  (define LEN (? Nat))
  (define E (? U))
  (unify (Vec LEN E) (<- v))
  (?/get LEN))

(define (≡ #:A [A (? U)] a b)
  (: A U)
  (unify (<- a) A)
  (: b A)
  (tt `(≡ ,A ,a ,b) U))
(define (refl #:A [A (? U)] #:a [a (? A)])
  (tt 'refl (≡ a a)))

(module+ test
  (check-equal? (pretty (s (s (z))))
                '(: (s (: (s (: z (: Nat U))) (: Nat U))) (: Nat U)))
  (check-equal? (pretty (List Nat))
                '(: (List (: Nat U)) U))
  (check-equal? (pretty (:: (s (z)) (:: (z) (nil))))
                '(:
                  (::
                   (: (s (: z (: Nat U))) (: Nat U))
                   (:
                    (:: (: z (: Nat U)) (: nil (: (List (: Nat U)) U)))
                    (: (List (: Nat U)) U)))
                  (: (List (: Nat U)) U)))
  (check-equal? (pretty (vec:: (z) (vecnil)))
                '(:
                  (vec:: (: z (: Nat U)) (: vecnil (: (Vec (: z (: Nat U)) (: Nat U)) U)))
                  (: (Vec (: (s (: z (: Nat U))) (: Nat U)) (: Nat U)) U)))

  (check-equal? (pretty (vec/length (vec:: (z) (vec:: (z) (vecnil)))))
                '(: (s (: (s (: z (: Nat U))) (: Nat U))) (: Nat U)))

  (define (sym #:A [A (? U)] #:x [x (? A)] #:y [y (? A)]
               [P1 (? (≡ x y))])
    (unify (refl) P1)
    (let ([r (refl)])
      (unify (≡ y x) (<- r))
      r))
  (pretty (sym))

  (define (plus m n)
    (: m Nat)
    (: n Nat)
    (match (tt-tm (?/get m))
      ['z n]
      [`(s ,m-)
       (s (plus m- n))]))

  (check-equal? (pretty (plus (s (z)) (s (z))))
                '(: (s (: (s (: z (: Nat U))) (: Nat U))) (: Nat U)))

  (define (+0/Nat #:x [x (? Nat)])
    (let ([r (refl)])
      (unify (≡ (plus (z) x) x) (<- r))
      r))
  (pretty (+0/Nat)))
