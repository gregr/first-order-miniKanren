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
(define (extend-sub x t sub diseq)
  (and (not (occurs? x t sub)) `((,x . ,t) . ,sub)))

(define empty-diseq '())

(define (extend-diseq =/=s diseq)
  (cons =/=s diseq))

(struct state (sub diseq) #:prefab)
(define empty-state (state empty-sub empty-diseq))

;; Unification
(define (unify/sub u v sub diseq)
  (let ((u (walk u sub)) (v (walk v sub)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) sub)
      ((var? u)                            (extend-sub u v sub diseq))
      ((var? v)                            (extend-sub v u sub diseq))
      ((and (pair? u) (pair? v))           (let ((sub (unify/sub (car u) (car v) sub diseq)))
                                             (and sub (unify/sub (cdr u) (cdr v) sub diseq))))
      (else                                (and (eqv? u v) sub)))))
(define (unify u v st)
  (let ((sub (unify/sub u v (state-sub st) (state-diseq st))))
    (and sub (cons (state sub (state-diseq st)) #f))))

;; Disunification
#;(define (disunify/sub u v sub diseq)
  (let ((u (walk u sub)) (v (walk v sub)))
    (cond
     ((and (var? u) (var? v) (var=? u v)) #f)
     ((var? u)                            (if (occurs? u v sub) diseq (extend-diseq (list (cons u v)) diseq)))
     ((var? v)                            (if (occurs? v u sub) diseq (extend-diseq (list (cons u v)) diseq)))
     ((and (pair? u) (pair? v))           (let* ((diseq1 (disunify/sub (car u) (car v) sub '()))
                                                 (diseq2 (disunify/sub (cdr u) (cdr v) sub '())))
                                             (match (list diseq1 diseq2)
                                               ((list '() _) diseq)
                                               ((list _ '()) diseq)
                                               ((list #f #f) #f)
                                               ((list diseq1 #f) (extend-diseq (car diseq1) diseq))
                                               ((list #f diseq2) (extend-diseq (car diseq2) diseq))
                                               ((list diseq1 diseq2) (extend-diseq (append (car diseq1) (car diseq2)) diseq)))))
     ((eqv? u v)                          #f)
     (else                                diseq))))

(define (disunify/sub u v st)
  (let* ((sub (state-sub st))
         (diseq (state-diseq st))
         (newsub (unify u v st)))
    (cond
      ((not newsub) diseq)
      ((eq? sub newsub) #f)
      (else (extend-diseq (disunify-helper sub newsub) diseq)))))

(define (disunify-helper sub newsub)
  (cond
    ((eq? sub newsub) '())
    (else (cons (car newsub) (disunify-helper sub (cdr newsub))))))

(define (disunify u v st)
  (let ((diseq (disunify/sub u v st)))
    (and diseq (cons (state (state-sub st) diseq) #f))))

;; Reification
(define (walk* tm sub)
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
  (reify initial-var st))
