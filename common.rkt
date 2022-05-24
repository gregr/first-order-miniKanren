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
  distypify
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
(define empty-distypes '())

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

(define (extend-distypes distype distypes)
  (cons distype distypes))

(define (var-type-remove t types)
  (remove t types (lambda (v type-constraint) (eq? v (car type-constraint)))))

(struct state (sub diseq types distypes) #:prefab)
(define empty-state (state empty-sub empty-diseq empty-types empty-distypes))

(define (state->stream state)
  (if state (cons state #f) #f))

;; Unification
(define (assign-var u v st)
  (let* ((types (state-types st))
         (u-type (var-type-ref u types))
         (types (if u-type (var-type-remove u types) types))
         (new-sub (extend-sub u v (state-sub st))))
    (and new-sub (let ((st (state new-sub (state-diseq st) types (state-distypes st))))
                   (if u-type
                       (typify u u-type st)
                       (state-simplify st))))))

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

;; Type constraints
(define (typify u type? st)
  (let ((u (walk u (state-sub st))))
    (if (var? u)
        (let ((u-type (var-type-ref u (state-types st))))
          (if u-type
              (and (eqv? type? u-type) st)
              (state-simplify (state (state-sub st)
                                     (state-diseq st)
                                     (extend-types u type? (state-types st))
                                     (state-distypes st)))))
        (and (type? u) st))))

;; Negation Constraints

(define (disprocify-helper x newx acc)
  (if (eqv? x newx)
      (reverse acc)
      (disprocify-helper x (cdr newx) (cons (car newx) acc))))

(define (disprocify newst st mode)
  (let* ((sub (state-sub st))
         (types (state-types st))
         (diseq (state-diseq st))
         (distypes (state-distypes st))
         (newsub (and newst (state-sub newst)))
         (newtypes (and newst (state-types newst))))
    (cond
      ((not newsub) st)                                      ; CASE: proc always fails
      ((and (eq? mode 'sub) (not (eq? newsub sub)))          ; CASE: proc succeeds under new sub
       (state sub (extend-diseq (disprocify-helper sub newsub '()) diseq) types distypes)) 
      ((and (eq? mode 'types) (not (eq? newtypes types)))    ; CASE: proc succeeds under new types
       (state sub diseq types (extend-distypes (disprocify-helper types newtypes '()) distypes)))
      (else #f))))                                           ; CASE: proc always succeeds

(define (state-simplify st)
  (let* ((st (and st (diseq-simplify st)))
         (st (and st (distype-simplify st))))
    st))

(define (diseq-simplify st)
  (let* ((sub (state-sub st))
         (diseq (state-diseq st))
         (types (state-types st))
         (distypes (state-distypes st))
         (st (state sub empty-diseq types distypes)))
    (foldl/and (lambda (=/=s st) (disunify (map car =/=s) (map cdr =/=s) st)) st diseq)))

(define (distype-simplify st)
  (let* ((sub (state-sub st))
         (diseq (state-diseq st))
         (types (state-types st))
         (distypes (state-distypes st))
         (st (state sub diseq types empty-distypes)))
    (foldl/and (lambda (!types st) (foldl/or (lambda (!type st) (distypify (car !type) (cdr !type) st)) st !types)) st distypes)))

(define (foldl/and proc acc lst)
  (if (null? lst)
      acc
      (let ((new-acc (proc (car lst) acc)))
        (and new-acc (foldl/and proc new-acc (cdr lst))))))

(define (foldl/or proc acc lst)
  (if (null? lst)
      acc
      (let ((new-acc (proc (car lst) acc)))
        (or new-acc (foldl/and proc new-acc (cdr lst))))))

;; Disunification

(define (disunify u v st)
  (disprocify (unify u v st) st 'sub))

;; Distypification

(define (distypify u type? st)
  (disprocify (typify u type? st) st 'types))

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
;(define (reify tm st) st)
(define (reify tm st)
  (define index -1)
  (let ((results (let loop ((tm tm) (st st))
                   (define t (walk tm (state-sub st))) ;; applies subsitiution
                   (cond ((pair? t) (loop (cdr t) (loop (car t) st))) ;; pair we loop through the first then use the new state to loop through second
                         ((var? t)  (set! index (+ 1 index)) ;; increasese the index
                                    (state (extend-sub t (reified-index index) (state-sub st)) (state-diseq st) (state-types st) (state-distypes st)))
                                    ;; returns new state with stylised variable index set
                         (else      st))))) ;; returns the state
        ;; so results goes through and substitues all vars with reified index, and returns that new state
    (let* ((walked-sub (walk* tm results)) ;; uses new state to walk through the term and subsitiute all vars with reified version
           (diseq (map (lambda (=/=) (cons (length =/=) =/=)) (state-diseq st))) ;; appends length of diseq? (i can't find a way to make this not eq to 1)
           (diseq (map cdr (sort diseq (lambda (x y) (< (car x) (car y)))))) ;; sort diseq by length, and remove length
           (st (state-simplify (state (state-sub st) diseq (state-types st) (state-distypes st)))) ;; simplifies state using the new sorted diseq
           (diseq (walk* (state-diseq st) results)) ;; reified walk the diseq
           (diseq (map pretty-diseq (filter-not contains-fresh? diseq))) ;; yeets all diseq that contain a variable, and pretties them?
           (diseq (map (lambda (=/=) (sort =/= term<?)) diseq)) ;; for all disequalities sort by term compairison
           (diseq (if (null? diseq) '() (list (cons '=/= diseq)))) ;; if diseq is empty return empty list, else return diseq
           (types (walk* (map pretty-types (state-types st)) results)) ;; now we pretty the types
           (distypes (walk* (map pretty-distypes (state-distypes st)) results)) ;; now we pretty the types
           (types (filter-not contains-fresh? types)) ;; removes all fresh
           (cxs (append types diseq distypes))) ;; creates final result
      (if (null? cxs) ;; if null, no constraints?
          walked-sub ;; return the walked term
          (Ans walked-sub (sort cxs term<?)))))) ;; otherwise return sub followed by sorted constraints

(define (reify/initial-var st)
  (reify initial-var st))

(define (term<? u v)
  (eqv? (term-compare u v) -1))

;; returns -1 if u < v, 0 if u = v, 1 if u > v
;; used for pretty
(define (term-compare u v)
  (cond
    ((eqv? u v) 0)
    ((null? u) -1)
    ((null? v) 1)
    ((not u) -1)
    ((not v) 1)
    ((eqv? u #t) -1)
    ((eqv? v #t) 1)
    ((symbol? u) (if (symbol? v) (if (symbol<? u v) -1 1) -1))
    ((symbol? v) 1)
    ((number? u) (if (number? v) (if (< u v) -1 1) -1))
    ((number? v) 1)
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

;; checks to see if x contains any fresh statments
(define (contains-fresh? x)
  (if (pair? x)
      (or (contains-fresh? (car x)) (contains-fresh? (cdr x))) ;; if a pair, check both sides
      (var? x))) ;; if no pair then: if it's variable we must have a fresh otherwise we don't

;; sorts by term compairison
(define (pretty-diseq =/=s)
  (map (lambda (=/=) (let ((x (car =/=)) (y (cdr =/=))) (if (term<? x y) (list x y) (list y x)))) =/=s))

;; turns typed terms into pretty strings
(define (pretty-types constraint) (list (type-check->sym (cdr constraint)) (car constraint)))

;; returns string type for symbol, string, and number
(define (type-check->sym pred)
  (cond
    ((eq? pred symbol?) 'sym)
    ((eq? pred string?) 'str)
    ((eq? pred number?) 'num)
    (error "Invalid type")))

;; turns typed terms into pretty strings
(define (pretty-distypes constraint)
(let* 
  ((constraint (car constraint)))
  (list (distype-check->sym (cdr constraint)) (car constraint)))
)

;; returns string type for symbol, string, and number
(define (distype-check->sym pred)
  (cond
    ((eq? pred symbol?) '!sym)
    ((eq? pred string?) '!str)
    ((eq? pred number?) '!num)
    (error "Invalid type")))