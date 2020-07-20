#lang nanopass

(module+ test
  (require rackunit))

(define variable? symbol?)
(define/match (type? t)
  [(`(-> ,t1 ,t2))
   #:when (and (type? t1) (type? t2))
   #t]
  [(t) #:when (variable? t)
       #t])

(define-language Inductive
  (terminals
   (type (typ))
   (variable (v c tv)))
  (Expr (e)
        v
        (e0 e1)
        (inductive v (c* typ*) ...)))

(struct t:ind (name constructor*) #:transparent)
(struct t:construction
  (name arg*) #:transparent)
(define (: tm typ)
  (unless (t:ind? typ) (error "unknown type"))
  (unless (t:construction? tm) (error "unknown term"))
  (ormap (λ (expected-c-name)
           (eqv? (t:construction-name tm)
                 expected-c-name))
         (t:ind-constructor* typ)))

(define (typ->construction name ty)
  (match ty
    [`(-> ,t1 ,t2)
     (λ (x)
       (unless (: x t1) (error (format "type mismatched, expected: ~a, but got: ~a" t1 (t->string x))))
       (t:construction name (list x)))]
    [t (t:construction name '())]))

(define/match (t->string t)
  [((t:construction name arg*))
   (if (empty? arg*)
       (symbol->string name)
       (string-join (cons (symbol->string name) (map t->string arg*)) " "
                    #:before-first "("
                    #:after-last ")"))]
  [(void) ""])

(define (eval t)
  (nanopass-case (Inductive Expr) t
                 [(inductive ,v
                             (,c* ,typ*) ...)
                  (cons `begin (cons
                   `(define ,v (t:ind ,v ,c*))
                   (map (λ (c-name c-typ)
                          `(define c-name (typ->construction c-name c-typ)))
                        c* typ*)))]
                 [(,e0 ,e1)
                  `(,(eval e0) ,(eval e1))]
                 [,v
                  `,v]))

(module+ test
  (define-parser ind-parser Inductive)

  (define (eval-result t)
    (eval (ind-parser t)))

  (eval-result `(inductive Nat [z Nat] [s (-> Nat Nat)])))
