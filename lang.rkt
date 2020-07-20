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

(define (typ->construction name ty ctx)
  (match ty
    [`(-> ,t1 ,t2)
     (λ (x)
       (unless (: x (hash-ref ctx t1)) (error (format "type mismatched, expected: ~a, but got: ~a" t1 (t->string x))))
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

(define (eval-list t* ctx)
  (match t*
    ['() (void)]
    [(cons h t*)
     (let ([e (eval h ctx)])
       (displayln (t->string (car e)))
       (eval-list t* (cdr e)))]))
(define (eval t ctx)
  (nanopass-case (Inductive Expr) t
                 [(inductive ,v
                             (,c* ,typ*) ...)
                  (let ([ctx (foldl (λ (c-name c-typ ctx)
                                      (hash-set ctx
                                                c-name
                                                (typ->construction c-name c-typ ctx)))
                                    (hash-set ctx v (t:ind v
                                                           c*))
                                    c*
                                    typ*)])
                    (cons void ctx))]
                 [(,e0 ,e1)
                  (let* ([e0 (eval e0 ctx)]
                         [e1 (eval e1 (cdr e0))])
                    (cons ((car e0) (car e1)) (cdr e1)))]
                 [,v
                  (cons (hash-ref ctx v) ctx)]))

(module+ test
  (define-parser ind-parser Inductive)

  (define (eval-t t*)
    (eval-list (map (λ (t) (ind-parser t)) t*) (make-immutable-hash)))

  (eval-t '((inductive Nat
                       [z Nat]
                       [s (-> Nat Nat)])
            z
            (s z)
            (s (s z))
            (inductive Bool
                       [true Bool]
                       [false Bool])
            (s true))))
