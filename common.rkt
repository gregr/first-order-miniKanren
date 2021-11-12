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

(define (walk-types t types)
  (cond
    ((number? t) number?)
    ((symbol? t) symbol?)
    ((string? t) string?)
    ((var? t) (let* ((xt (assf (lambda (x) (var=? t x)) types)))
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

(define (state->stream state)
  (if state (cons state #f) #f))

;; Unification
(define (assign-var u v st)
  (let* ((types (state-types st))
         (u-type (walk-types u types))
         (types (if u-type (reduce-types u types) types))
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
  (if (empty? lst)
      acc
      (let ((new-acc (proc (car lst) acc)))
        (if new-acc (foldl/and proc new-acc (cdr lst)) #f))))

;; Type constraints
(define (typify u type? st)
  (let ((u (walk u (state-sub st))))
    (if (var? u)
        (let ((u-type (walk-types u (state-types st))))
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
    (cond
      ((and (null? (state-diseq st)) (null? (state-types st))) (walk* tm results))
      ((null? (state-types st)) (Ans 
                                  (walk* tm results)
                                  (walk* (cons '=/= (map pretty-diseq (state-diseq st))) results)))
      ((null? (state-diseq st)) (Ans
                                  (walk* tm results)
                                  (walk* (map pretty-types (state-types st)) results)))
      (else (Ans (walk* tm results) 
                (list (walk* (map pretty-types (state-types st)) results) 
                      (walk* (cons '=/= (map pretty-diseq (state-diseq st))) results) ))))))
      ; ((and (null? (state-diseq st)) (null? (state-types st))) (walk* tm results))
      ; ((null? (state-types st)) (list 
      ;                             (walk* tm results)
      ;                             (walk* (cons '=/= (map pretty-diseq (state-diseq st))) results)))
      ; ((null? (state-diseq st)) (list
      ;                             (walk* tm results)
      ;                             (walk* (map pretty-types (state-types st)) results)))
      ; (else (list (walk* tm results) 
      ;             (walk* (map pretty-types (state-types st)) results) 
      ;             (walk* (cons '=/= (map pretty-diseq (state-diseq st))) results) )))))

(define (reify/initial-var st)
  (reify initial-var st))

(define (pretty-diseq =/=s) (map (lambda (=/=) (list (car =/=) (cdr =/=))) =/=s))
(define (pretty-types constraint) (list (type-check->sym (cdr constraint)) (car constraint)))

(define (type-check->sym pred)
  (cond
    ((eq? pred symbol?) 'sym)
    ((eq? pred string?) 'str)
    ((eq? pred number?) 'num)
    (error "Invalid type")))