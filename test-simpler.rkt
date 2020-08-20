#lang racket

(module+ test
  (require rackunit))

; helper
(define (pretty t)
  (match (?/get t)
    [`(,a* ...) (map pretty a*)]
    [`(,tm . ,ty) `(,(pretty tm) . ,(pretty ty))]
    [t t]))
(define (?/get p?) (if (parameter? p?) (p?) p?))

; U is greatest type level
(define U 'U)
(define (?/new ty) (make-parameter `(,(gensym '?) . ,ty)))
(define (occurs v t)
  (match t
    [`(,t* ...)
     (ormap (λ (t) (occurs v t)) t*)]
    (t (equal? v t))))
(define (unify t1 t2)
  (match* (t1 t2)
    [(_ (? parameter?))
     (unless (or (eqv? t1 (?/get t2)) (not (occurs (?/get t2) t1)))
       (error (format "~a occurs in ~a" (?/get t2) (?/get t1))))
     (t2 (?/get t1))]
    ; swap
    [((? parameter?) _) (unify t2 t1)]
    [(`(,tm1 . ,ty1) `(,tm2 . ,ty2))
     (unify ty1 ty2)
     (unify tm1 tm2)]
    ; not free variable, then we expect they are same type
    [(_ _) (ty= t1 t2)]))
(define (: term type)
  (unless (ty= (cdr (?/get term)) type)
    (error (format "~a is a ~a, not a ~a"
                   (pretty term)
                   (pretty (cdr (?/get term)))
                   (pretty type)))))
(define (<- t)
  (match t
    ['U U]
    [`(,term . ,type) type]))
(define (ty= t1 t2)
  (unless (equal? (pretty t1) (pretty t2))
    (error (format "~a != ~a" (pretty t1) (pretty t2)))))

(define Bool '(Bool . U))
(define (true) `(true . ,Bool))
(define (false) `(false . ,Bool))

(define Nat '(Nat . U))
(define (z) `(z . ,Nat))
(define (s n)
  (: n Nat)
  `((s ,n) . ,Nat))

(define (plus m n)
  (: m Nat)
  (: n Nat)
  (match (car m)
    ['z n]
    [`(s ,m-)
     (s (plus m- n))]))

(define (List A)
  (: A U)
  `((List ,A) . U))
(define (nil) `(nil . ,(List (?/new U))))
(define (:: #:A [A (?/new U)] a lst)
  (unify A (<- a))
  (: (?/get A) U)
  (unify (List (?/get A)) (<- lst))
  `((:: ,a ,lst) . ,(pretty (List A))))

(define (Vec LEN E)
  (: LEN Nat)
  (: E U)
  `((Vec ,LEN ,E) . U))
(define (vecnil) `(vecnil . ,(Vec (z) (?/new U))))
(define (vec:: #:E [E (?/new U)] #:LEN [LEN (?/new Nat)] e v)
  (unify E (<- e))
  (: (?/get E) U)
  (unify (Vec LEN (?/get E)) (<- v))
  `((vec:: ,e ,v) . ,(Vec (s LEN) E)))

(define (vec/length v)
  (define LEN (?/new Nat))
  (define E (?/new U))
  (unify (Vec LEN E) (<- v))
  (?/get LEN))

(module+ test
  (check-equal? (s (s (z))) '((s ((s (z Nat . U)) Nat . U)) Nat . U))
  (check-equal? (List Nat) '((List (Nat . U)) . U))
  (check-equal? (pretty (:: (s (z)) (:: (z) (nil))))
                '((::
                   ((s (z Nat . U)) Nat . U)
                   ((:: (z Nat . U) (nil (List (Nat . U)) . U)) (List (Nat . U)) . U))
                  (List (Nat . U)) . U))
  (check-equal? (pretty (vec:: (z) (vecnil)))
                '((vec::
                   (z Nat . U)
                   (vecnil (Vec (z Nat . U) (Nat . U)) . U))
                  (Vec ((s (z Nat . U)) Nat . U) (Nat . U)) . U))

  (check-equal? (pretty (plus (s (z)) (s (z))))
                '((s ((s (z Nat . U)) Nat . U)) Nat . U))

  (check-equal? (pretty (vec/length (vec:: (z) (vec:: (z) (vecnil)))))
                '((s ((s (z Nat . U)) Nat . U)) Nat . U)))
