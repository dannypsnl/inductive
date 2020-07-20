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
   (variable (v c)))
  (Expr (e)
        v
        (inductive v (c* typ*) ...)))

;;; improve t:ind, add more info for type checking
(struct t:ind ())

(define (eval t ctx)
  (nanopass-case (Inductive Expr) t
                 [(inductive ,v
                             (,c* ,typ*) ...)
                  (let ([ctx (hash-set ctx v (t:ind))])
                    (foldl (λ (c-name typ ctx)
                             ;;; TODO: convert typ to term
                             (hash-set ctx c-name typ))
                           ctx
                           c*
                           typ*))]
                 [else t]))

(module+ test
  (define-parser ind-parser Inductive)
  (ind-parser '(inductive Nat
                          [z Nat]
                          [s (-> Nat Nat)]))
  (ind-parser 'z)

  (define (eval-t t)
    (eval (ind-parser t) (make-immutable-hash)))

  (eval-t '(inductive Nat
                      [z Nat]
                      [s (-> Nat Nat)])))
