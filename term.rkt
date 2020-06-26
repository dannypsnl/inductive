#lang typed/racket

(require "syntax.rkt")
(require/typed typed/racket
               (srcloc->string (srcloc -> String)))

(define-type t
  (U t:var
     t:arrow))
(struct t:var ([idx : Integer]) #:transparent)
(struct t:arrow ([t1 : t] [t2 : t]) #:transparent)

(define (t->string [t : t]
                   [ctx : context])
  : String
  (match t
    [(t:var idx) (t->string (list-ref ctx idx) ctx)]
    [(t:arrow t1 t2)
     (format "~s -> ~s"
             (t->string t1 ctx)
             (t->string t2 ctx))]))

(: run (->* [(Listof syn:stmt)]
            [context name-map]
            Void))
(define (run s*
             [ctx '()]
             [name-map (make-immutable-hash '())])
  (match s*
    [(cons stmt stmt*)
     (run stmt* (run-stmt stmt ctx name-map))]
    ['() (void)]))
(define (run-stmt [s : syn:stmt]
                  [ctx : context]
                  [name-map : name-map])
  : context
  (match s
    ;;; lookup context to get type of term?
    [(syn:stmt:type-of _ term)
     (printf "~s~n" (t->string (type-of-term term ctx name-map) ctx))
     ctx]
    ;;; add new type and constructor into context
    [(syn:stmt:inductive pos name constructor*)
     ;;; TODO: introduce new type
     ctx]))
(define (type-of-term [term : syn:term]
                      [ctx : context]
                      [name-map : name-map])
  : t
  (match term
    [(syn:var pos name)
     (list-ref ctx
               (hash-ref name-map name
                         (λ () (error (format "~s: undefined variable `~s`"
                                              (srcloc->string pos)
                                              name)))))]
    [(syn:λ pos name typ body)
     (define param-typ : t (syn:type->t typ ctx name-map))
     (define ctx : context (append ctx (list param-typ)))
     (t:arrow param-typ
              (type-of-term body
                            ctx
                            (hash-set name-map name (length ctx))))]))
(define (syn:type->t [typ : syn:type]
                     [ctx : context]
                     [name-map : name-map])
  : t
  (match typ
    [(syn:var pos name)
     (list-ref ctx (hash-ref name-map name
                             (λ () (error (format "~s: undefined variable `~s`"
                                                  (srcloc->string pos)
                                                  name)))))]
    [(syn:arrow pos t1 t2)
     (t:arrow (syn:type->t t1 ctx name-map)
              (syn:type->t t2 ctx name-map))]))

(define-type name-map (Immutable-HashTable String Integer))
(define-type context (Listof t))

(module+ test
  (define pos (make-srcloc 'here 1 0 1 0))
  (run (list
        (syn:stmt:inductive pos "Nat"
                            (list
                             (syn:constructor pos "z" (syn:var pos "Nat"))
                             (syn:constructor pos "s" (syn:arrow pos (syn:var pos "Nat") (syn:var pos "Nat")))))
        (syn:stmt:type-of pos (syn:var pos "z")))))
