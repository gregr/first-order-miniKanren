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
(define (walk-types t sub types)
  (cond
    ((number? t) number?)
    ((symbol? t) symbol?)
    ((string? t) string?)
    ((var? t) (let* ((walked-t (walk t sub))
                     (xt (assf (lambda (x) (var=? walked-t x)) types)))
                (if xt (cdr xt) #f)))
    (else #f)))


(define (extend-sub x t sub)
  (and (not (occurs? x t sub)) `((,x . ,t) . ,sub)))

(define (extend-diseq =/=s diseq)
  (cons =/=s diseq))

(define (extend-types x t types)
  `((,x . ,t) . ,types))

(define (reduce-types t types)
  (filter (lambda (type-constraint) (not (eq? t (car type-constraint)))) types))


(struct state (sub diseq types) #:prefab)
(define empty-state (state empty-sub empty-diseq empty-types))

#|
(define (simplify-state t st [add-type #f])
  (if st
      (let ((type-simplified (type-simplify t st add-type)))
        (if type-simplified (diseq-simplify type-simplified) #f))
      #f))

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
|#

;; Unification
#|(define (unify/sub u v sub)
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
    (and sub (simplify-state v (simplify-state u (state sub diseq types))))))
|#
(define (move-type u v st)
  (let* ((types (state-types st))
         (u-type (walk-types u types))
         (v-type (walk-types v types)))
    (cond
      ((and u-type v-type) (if (eqv? u-type v-type)
                               (reduce-types u types)
                               #f)) 
      ((and (var? v) u-type) (typify v u-type (state (state-sub st) (state-diseq st) (reduce-types u types)))) 
      (u-type (reduce-types u types))
      (else types))))  
    

(define (unify-helper u v st)
  (let* ((sub (state-sub st))
        (types (state-types st))
        (u (walk u sub)) (v (walk v sub)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) st)
      ((var? u)                            (let ((new-types (move-type u v st))) (if new-types (state (extend-sub u v sub) (state-diseq st) new-types) #f)))
      ((var? v)                            (let ((new-types (move-type v u st))) (if new-types (state (extend-sub v u sub) (state-diseq st) new-types) #f)))
      ((and (pair? u) (pair? v))           (let ((st (unify-helper (car u) (car v) st)))
                                             (and st (unify-helper (cdr u) (cdr v) st))))
      (else                                (and (eqv? u v) st)))))
(define (unify u v st)
  (let ((st (unify-helper u v (state-sub st))))
    (and st (diseq-simplify st))))


#|(define (unify u v st)
  (let ((sub (state-sub st))
        (diseq (state-diseq st))
        (types (state-types st))
        (u (walk u sub))
        (v (walk v sub)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) st)
      ((var? u)                            (extend-sub u v sub)) ; TODO : fix types
      ((var? v)                            (extend-sub v u sub)) ; TODO : fix types
      ((and (pair? u) (pair? v))           (let ((st (unify (car u) (car v) st)))
                                             (and st (unify (cdr u) (cdr v) st))))
      (else                                (and (eqv? u v) st)))))
|#

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
      (else (diseq-simplify (state sub (extend-diseq (disunify-helper sub newsub '()) diseq) types))))))

(define (diseq-simplify st)
  (let* ((sub (state-sub st))
         (diseq (state-diseq st))
         (types (state-types st))
         (new-diseq (filter (lambda (z) (andmap (lambda (x) (inner-diseq-simplifier x st)) z)) diseq))
         (new-new-diseq (map (lambda (x) (filter (lambda (y) (diseq-element-simplifier y st)) x)) new-diseq)))
    (if (ormap empty? new-diseq)
        #f
        (state sub new-new-diseq types))))

(define (inner-diseq-simplifier d st)
  (let* ((sub (state-sub st))
         (u (walk (car d) sub))
         (v (walk (cdr d) sub))
         (u-type (walk-types u sub (state-types st)))
         (v-type (walk-types v sub (state-types st))))
    (cond
      ((and (not (var? u)) (not (var? v))) (eqv? u v))
      (else (or (not u-type) (not v-type) (eqv? u-type v-type))))))

(define (diseq-element-simplifier d st)
    (let* ((sub (state-sub st))
           (u (walk (car d) sub))
           (v (walk (cdr d) sub)))
      (not (eqv? u v))))
#|    (cond
      ((eqv? u v) #f)
      ((and (var? u) (var? v)) (or (eqv? u-type v-type) (not u-type) (not v-type)))
      ;((and (not (var? u)) (not (var? v))) #f) ; TODO do we need this?
      ((var? u) (or (not u-type) (u-type v)))
      ((var? v) (or (not v-type) (v-type u)))
      (else #t))))|#

;; Type constraints
(define (typify type? u st)
  (let* ((u (walk u (state-sub st)))
         (ct (walk-types u (state-sub st) (state-types st))))
    (cond
      ((or (type? u) (eq? type? ct)) st)
      ((and (not ct) (var? u)) (state (state-sub st) (state-diseq st) (extend-types u type? (state-types st))))
      (else #f))))

#|
(define (type-simplify t st [add-type #f])
  (let* ((sub (state-sub st))
         (types (state-types st))
         (diseqs (state-diseq st))
         (type-t (and (var? t) (assf (lambda (x) (var=? t x)) types)))
         (xt (and (var? t) (assf (lambda (x) (var=? t x)) sub)))
         (new-types (if type-t (reduce-types t types) types)))
    (cond
      ((and (not (var? t)) (not add-type)) st)
      ;((not (var? t)) (if (add-type t) st #f)) ; TODO is this needed?
      ((and add-type type-t (not (eq? add-type type-t))) #f)
      ((and xt (not type-t)) (type-simplify (cdr xt) st))
      ((and xt type-t) (type-simplify (cdr xt) (state sub diseqs new-types) (cdr type-t)))
      ((and (var? t) add-type) (state sub diseqs (cons (cons t add-type) new-types)))
      ((var? t) st)
      (else (state sub diseqs new-types))
    )))
|#

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