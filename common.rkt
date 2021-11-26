#lang racket
(provide
  (struct-out var)
  initial-var
  var/fresh
  (struct-out state)
  empty-state
  state->stream
  unify
  disunify
  typify
  walk*
  reify
  reify/initial-var)

;; Logic variables
(struct var (name index) #:prefab)
(define (var=? x1 x2)
  (= (var-index x1) (var-index x2)))
(define initial-var (var #f 0))
(define var/fresh
  (let ((index 0))
    (lambda (name) (set! index (+ 1 index))
      (var name index))))

;; States
(define empty-sub '())
(define empty-diseq '())
(define empty-types '())

(define (walk t sub)
  (let ((xt (and (var? t) (assf (lambda (x) (var=? t x)) sub))))
    (if xt (walk (cdr xt) sub) t)))

(define (occurs? x t sub)
  (cond ((pair? t) (or (occurs? x (walk (car t) sub) sub)
                       (occurs? x (walk (cdr t) sub) sub)))
        ((var? t)  (var=? x t))
        (else      #f)))

(define (var-type-ref t types)
    (let* ((xt (assf (lambda (x) (var=? t x)) types)))
      (and xt (cdr xt))))

(define (extend-sub x t sub)
  (and (not (occurs? x t sub)) `((,x . ,t) . ,sub)))

(define (extend-diseq =/=s diseq)
  (cons =/=s diseq))

(define (extend-types x t types)
  `((,x . ,t) . ,types))

(define (var-type-remove t types)
  (remove t types (lambda (v type-constraint) (eq? v (car type-constraint)))))

(struct state (sub diseq types) #:prefab)
(define empty-state (state empty-sub empty-diseq empty-types))

(define (state->stream state)
  (if state (cons state #f) #f))

;; Unification
(define (assign-var u v st)
  (let* ((types (state-types st))
         (u-type (var-type-ref u types))
         (types (if u-type (var-type-remove u types) types))
         (new-sub (extend-sub u v (state-sub st))))
    (and new-sub (let ((st (state new-sub (state-diseq st) types)))
                   (if u-type
                       (typify u u-type st)
                       (diseq-simplify st))))))

(define (unify u v st)
  (let* ((sub (state-sub st))
         (u (walk u sub))
         (v (walk v sub)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) st)
      ((var? u)                            (assign-var u v st))
      ((var? v)                            (assign-var v u st))
      ((and (pair? u) (pair? v))           (let ((st (unify (car u) (car v) st)))
                                             (and st (unify (cdr u) (cdr v) st))))
      (else                                (and (eqv? u v) st)))))

;; Disunification
(define (disunify-helper sub newsub acc)
  (cond
    ((eq? sub newsub) (reverse acc))
    (else (disunify-helper sub (cdr newsub) (cons (car newsub) acc)))))

(define (disunify u v st)
  (let* ((sub (state-sub st))
         (diseq (state-diseq st))
         (types (state-types st))
         (unify-answer (unify u v st))
         (newsub (if unify-answer (state-sub unify-answer) #f)))
    (cond
      ((not newsub) st)
      ((eq? newsub sub) #f)
      (else (state sub (extend-diseq (disunify-helper sub newsub '()) diseq) types)))))

(define (diseq-simplify st)
  (let* ((sub (state-sub st))
         (diseq (state-diseq st))
         (types (state-types st))
         (st (state sub empty-diseq types)))
    (foldl/and (lambda (=/=s st) (disunify (map car =/=s) (map cdr =/=s) st)) st diseq)))

(define (foldl/and proc acc lst)
  (if (null? lst)
      acc
      (let ((new-acc (proc (car lst) acc)))
        (and new-acc (foldl/and proc new-acc (cdr lst))))))

;; Type constraints
(define (typify u type? st)
  (let ((u (walk u (state-sub st))))
    (if (var? u)
        (let ((u-type (var-type-ref u (state-types st))))
          (if u-type
              (and (eqv? type? u-type) st)
              (diseq-simplify (state (state-sub st)
                                     (state-diseq st)
                                     (extend-types u type? (state-types st))))))
        (and (type? u) st))))


;; Reification
(struct Ans (term constraint) #:prefab)

(define (walk* tm st)
  (let* ((sub (state-sub st)) (tm (walk tm sub)))
    (if (pair? tm)
        `(,(walk* (car tm) st) .  ,(walk* (cdr tm) st))
        tm)))
(define (reified-index index)
  (string->symbol
    (string-append "_." (number->string index))))
(define (reify tm st)
  (define index -1)
  (let ((results (let loop ((tm tm) (st st))
                   (define t (walk tm (state-sub st)))
                   (cond ((pair? t) (loop (cdr t) (loop (car t) st)))
                         ((var? t)  (set! index (+ 1 index))
                                    (state (extend-sub t (reified-index index) (state-sub st)) (state-diseq st) (state-types st)))
                         (else      st)))))
    (let* ((walked-sub (walk* tm results))
           (diseq (map (lambda (=/=) (cons (length =/=) =/=)) (state-diseq st)))
           (diseq (map cdr (sort diseq (lambda (x y) (< (car x) (car y))))))
           (st (diseq-simplify (state (state-sub st) diseq (state-types st))))
           (diseq (walk* (map pretty-diseq (state-diseq st)) results))
           (diseq (filter-not contains-fresh? diseq))
           (diseq (map (lambda (=/=) (sort =/= term<?)) diseq))
           (diseq (if (null? diseq) '() (list (cons '=/= diseq))))
           (types (walk* (map pretty-types (state-types st)) results))
           (types (filter-not contains-fresh? types))
           (cxs (append types diseq)))
      (if (null? cxs)
          walked-sub
          (Ans walked-sub (sort cxs term<?))))))

(define (reify/initial-var st)
  (reify initial-var st))

(define (term<? u v)
  (eqv? (term-compare u v) -1))

(define (term-compare u v)
  (cond
    ((eqv? u v) 0)
    ((null? u) -1)
    ((null? v) 1)
    ((not u) -1)
    ((not v) 1)
    ((eqv? u #t) -1)
    ((eqv? v #t) 1)
    ((number? u) (if (number? v) (if (< u v) -1 1) -1))
    ((number? v) 1)
    ((symbol? u) (if (symbol? v) (if (symbol<? u v) -1 1) -1))
    ((symbol? v) 1)
    ((string? u) (if (string? v) (if (string<? u v) -1 (if (string=? u v) 0 1)) -1))
    ((string? v) 1)
    ((pair? u) (if (pair? v) 
                   (let ((compared-cars (term-compare (car u) (car v))))
                     (if (eqv? compared-cars 0)
                         (term-compare (cdr u) (cdr v))
                         compared-cars))
                   -1))
    ((pair? v) 1)
    (else 1)))

(define (contains-fresh? x)
  (if (pair? x)
      (or (contains-fresh? (car x)) (contains-fresh? (cdr x)))
      (var? x)))
(define (pretty-diseq =/=s) (map (lambda (=/=) (list (car =/=) (cdr =/=))) =/=s))
(define (pretty-types constraint) (list (type-check->sym (cdr constraint)) (car constraint)))

(define (type-check->sym pred)
  (cond
    ((eq? pred symbol?) 'sym)
    ((eq? pred string?) 'str)
    ((eq? pred number?) 'num)
    (error "Invalid type")))