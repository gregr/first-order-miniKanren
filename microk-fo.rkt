#lang racket
(provide
  (all-from-out "common.rkt")
  (struct-out disj)
  (struct-out conj)
  (struct-out relate)
  (struct-out ==)
  (struct-out mplus)
  (struct-out bind)
  (struct-out pause)
  step
  mature
  mature?)

(require "common.rkt")

;; first-order microKanren
(struct disj   (g1 g2)                  #:prefab)
(struct conj   (g1 g2)                  #:prefab)
(struct relate (thunk description)      #:prefab)
(struct ==     (t1 t2)                  #:prefab)
(struct bind   (bind-s bind-g)          #:prefab)
(struct mplus  (mplus-s1 mplus-s2)      #:prefab)
(struct pause  (pause-state pause-goal) #:prefab)

(define (mature? s) (or (not s) (pair? s)))
(define (mature s)
  (if (mature? s) s (mature (step s))))

(define (start st g)
  (match g
    ((disj g1 g2)
     (step (mplus (pause st g1)
                  (pause st g2))))
    ((conj g1 g2)
     (step (bind (pause st g1) g2)))
    ((relate thunk _)
     (pause st (thunk)))
    ((== t1 t2) (unify t1 t2 st))))

(define (step s)
  (match s
    ((mplus s1 s2)
     (let ((s1 (if (mature? s1) s1 (step s1))))
       (cond ((not s1) s2)
             ((pair? s1)
              (cons (car s1)
                    (mplus s2 (cdr s1))))
             (else (mplus s2 s1)))))
    ((bind s g)
     (let ((s (if (mature? s) s (step s))))
       (cond ((not s) #f)
             ((pair? s)
              (step (mplus (pause (car s) g)
                           (bind (cdr s) g))))
             (else (bind s g)))))
    ((pause st g) (start st g))
    (_            s)))
