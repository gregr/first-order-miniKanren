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
(define (walk t sub)
  (let ((xt (and (var? t) (assf (lambda (x) (var=? t x)) sub))))
    (if xt (walk (cdr xt) sub) t)))
(define (occurs? x t sub)
  (cond ((pair? t) (or (occurs? x (walk (car t) sub) sub)
                       (occurs? x (walk (cdr t) sub) sub)))
        ((var? t)  (var=? x t))
        (else      #f)))

; new stuff start
(define (implies-one? sub v u)
  (let ((u (walk u sub)) (v (walk v sub))) (eqv? u v)))

(define (implies? sub =/=s)
  (andmap (lambda (x) (implies-one? sub (car x) (cdr x))) =/=s))

(define (correct-type? sub type-constraint)
  (let ((u (walk (cdr type-constraint) sub))
        (type? (car type-constraint))) 
      (or (var? u) (type? u))))

(define (contradicts? sub diseq types)
  (or (ormap (lambda (x) (implies? sub x)) diseq) (ormap (lambda (x) (not (correct-type? sub x))) types)))

; new stuff end

(define (extend-sub x t sub)
  (and (not (occurs? x t sub)) `((,x . ,t) . ,sub)))

(define empty-diseq '())

(define (extend-diseq =/=s diseq)
  (cons =/=s diseq))

(define empty-types '())

(define (extend-types constraint types)
  (cons constraint types))

(struct state (sub diseq types) #:prefab)
(define empty-state (state empty-sub empty-diseq empty-types))

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
    (and sub (not (contradicts? sub diseq types)) (cons (state sub diseq types) #f)))) 

;; Disunification
(define (disunify-helper sub newsub acc)
  (cond
    ((eq? sub newsub) (reverse acc))
    (else (disunify-helper sub (cdr newsub) (cons (car newsub) acc)))))

(define (disunify u v st)
  (let* ((sub (state-sub st))
         (diseq (state-diseq st))
         (types (state-types st))
         (newsub (state-sub (car (unify u v st)))))
    (cond
      ((not newsub) st)
      ((eq? newsub sub) #f)
      (else (cons (state sub (extend-diseq (disunify-helper sub newsub '()) diseq) types) #f)))))

;; Type constraints
(define (type-check type u st)
  ;(cons st #f))
  (let* ((sub (state-sub st))
         (diseq (state-diseq st))
         (u (walk u sub)))
    (cond
      ((type u) st)
      ((var? u) (cons (state sub diseq (extend-types (cons type u) (state-types st))) #f))
      (else #f))))

;; Reification
#|(define (walk* tm sub)
  (let ((tm (walk tm sub)))
    (if (pair? tm)
        `(,(walk* (car tm) sub) .  ,(walk* (cdr tm) sub))
        tm)))
(define (reified-index index)
  (string->symbol
    (string-append "_." (number->string index))))
(define (reify tm st)
  (define index -1)
  (walk* tm (let loop ((tm tm) (sub (state-sub st)))
              (define t (walk tm sub))
              (cond ((pair? t) (loop (cdr t) (loop (car t) sub)))
                    ((var? t)  (set! index (+ 1 index))
                               (extend-sub t (reified-index index) sub))
                    (else      sub)))))
(define (reify/initial-var st)
  (reify initial-var st))|#

(struct A (term diseq-constraints type-constraints) #:prefab)
;(struct V (index) #:prefab)

;; Reification
(define (walk* tm st)
  (let* ((sub (state-sub st)) (tm (walk tm sub)))
    (if (pair? tm)
        `(,(walk* (car tm) st) .  ,(walk* (cdr tm) st))
        tm)))
;(define (reified-index index)
;  (V index))
(define (reified-index index)
  (string->symbol
    (string-append "_." (number->string index))))
(define (reify tm st)
  (define index -1)
  (let ((x (let loop ((tm tm) (st st))
              (define t (walk tm (state-sub st)))
              (cond ((pair? t) (loop (cdr t) (loop (car t) st)))
                    ((var? t)  (set! index (+ 1 index))
                               (state (extend-sub t (reified-index index) (state-sub st)) (state-diseq st) (state-types st)))
                    (else      st)))))
    (cond
      ((and (null? (state-diseq st)) (null? (state-types st))) (walk* tm x))
      ((null? (state-types st)) (A (walk* tm x) (walk* (cons '=/= (map pretty-diseq (state-diseq st))) x) '()))
      ((null? (state-diseq st)) (A (walk* tm x) '() (walk* (map pretty-types (state-types st)) x)))
      (else (A (walk* tm x) (walk* (cons '=/=' (map pretty-diseq (state-diseq st))) x) (walk* (map pretty-types (state-types st)) x))))))
    ;(if (null? (state-diseq st)) (walk* tm x) (A (walk* tm x) (walk* (cons '=/= (map pretty-diseq (state-diseq st))) x)))))

(define (reify/initial-var st)
  (reify initial-var st))

(define (pretty-diseq =/=s) (map (lambda (=/=) (list (car =/=) (cdr =/=))) =/=s))
(define (pretty-types constraint) (list (type-check->sym (car constraint)) (cdr constraint)))

(define (type-check->sym pred)
  (cond
    ((eq? pred symbol?) 'sym)
    ((eq? pred string?) 'str)
    ((eq? pred number?) 'num)
    (else 'not-implemented)))