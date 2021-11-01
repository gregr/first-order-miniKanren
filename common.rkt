#lang racket
(provide
  (struct-out var)
  initial-var
  var/fresh
  (struct-out state)
  empty-state
  state-sub
  unify
  disunify
  type-check
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
(define (walk-types t sub types)
  (let* ((walked-t (walk t sub))
         (xt (assf (lambda (x) (var=? walked-t x)) types)))
    (if xt (car xt) t)))


(define (extend-sub x t sub)
  (and (not (occurs? x t sub)) `((,x . ,t) . ,sub)))

(define (extend-diseq =/=s diseq)
  (cons =/=s diseq))

(define (extend-types x type? types)
  (and (not (occurs? x type? types)) `((,x . ,type?) . ,types)))

(define (reduce-types t types)
  (filter (lambda (type-constraint) (not (eq? t (car type-constraint)))) types))


(struct state (sub diseq types) #:prefab)
(define empty-state (state empty-sub empty-diseq empty-types))

;; Contradiction checks
(define (implies-one? sub v u)
  (let ((u (walk u sub)) (v (walk v sub))) (eqv? u v)))

(define (implies? sub =/=s)
  (andmap (lambda (x) (implies-one? sub (car x) (cdr x))) =/=s))

(define (correct-type? sub type-constraint)
  (let ((u (walk (car type-constraint) sub))
        (type? (cdr type-constraint))) 
      (or (var? u) (type? u))))

(define (contradicts? sub diseq types)
  (or (ormap (lambda (x) (implies? sub x)) diseq) (ormap (lambda (x) (not (correct-type? sub x))) types)))

;; Unification
(define (unify/sub u v sub)
  (let ((u (walk u sub)) (v (walk v sub)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) sub)
      ((var? u)                            (extend-sub u v sub))
      ((var? v)                            (extend-sub v u sub))
      ((and (pair? u) (pair? v))           (let ((sub (unify/sub (car u) (car v) sub)))
                                             (and sub (unify/sub (cdr u) (cdr v) sub))))
      (else                                (and (eqv? u v) sub)))))
(define (unify u v st)
  (let ((sub (unify/sub u v (state-sub st)))
        (diseq (state-diseq st))
        (types (state-types st)))
    (and sub (not (contradicts? sub diseq types)) (state sub diseq types)))) 

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

;; Type constraints
(define (type-check type? u st)
  (let* ((u (walk u (state-sub st)))
         (ct (walk u (state-types st))))
    (cond
      ((or (type? u) (eq? type? ct)) st) 
      ((not (var? ct)) #f)                   
      ((var? u) (state (state-sub st) (state-diseq st) (extend-sub u type? (state-types st))))
      (else #f))))

(define (type-simplify t st [add-type #f])
  (let* ((sub (state-sub st))
         (types (state-types st))
         (diseqs (state-diseq st))
         (type-t (assf (lambda (x) (var=? t x)) types))
         (xt (and (var? t) (assf (lambda (x) (var=? t x)) sub)))
         (new-types (if type-t (reduce-types t types) types)))
    (cond
      ((and add-type type-t (not (eq? add-type type-t))) #f)
      ((and xt (not type-t)) (type-simplify (cdr xt) st))
      ((and xt type-t) (type-simplify (cdr xt) (state sub diseqs new-types) (cdr type-t)))
      ((and (var? t) add-type) (state sub diseqs (cons (cons t add-type) new-types)))
      (else (state sub diseqs new-types))
    )))

;; Reification
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
    (cond
      ((and (null? (state-diseq st)) (null? (state-types st))) (walk* tm results))
      ((null? (state-types st)) (list (walk* tm results) (walk* (cons '=/= (map pretty-diseq (state-diseq st))) results)))
      ((null? (state-diseq st)) (list (walk* tm results) (walk* (map pretty-types (state-types st)) results)))
      (else (list (walk* tm results) (walk* (map pretty-types (state-types st)) results) (walk* (cons '=/= (map pretty-diseq (state-diseq st))) results) )))))

(define (reify/initial-var st)
  (reify initial-var st))

(define (pretty-diseq =/=s) (map (lambda (=/=) (list (car =/=) (cdr =/=))) =/=s))
(define (pretty-types constraint) (list (type-check->sym (cdr constraint)) (car constraint)))

(define (type-check->sym pred)
  (cond
    ((eq? pred symbol?) 'sym)
    ((eq? pred string?) 'str)
    ((eq? pred number?) 'num)
    (else 'not-implemented)))