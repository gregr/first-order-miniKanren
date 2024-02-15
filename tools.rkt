#lang racket
(provide
 (all-from-out "mk-fo.rkt")
 prune/stream
 prune/goal
 dnf/stream
 dnf/goal

 strip/stream
 strip/state
 pretty/state
 pretty/stream
 pretty/goal

 parallel-step-simple
 parallel-step

 mature/step
 stream-take/step
 run/step-simplify
 run/step
 run*/step
 step

 explore/stream
 explore)

(require "microk-fo.rkt")
(require "mk-fo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Propagate shallow constraints and prune failures.
(define (prune/stream s)
  (match s
    ((mplus s1 s2) (match (prune/stream s1)
                     (#f (prune/stream s2))
                     (s1 (match (prune/stream s2)
                           (#f s1)
                           (s2 (mplus s1 s2))))))
    ((bind s g)    (match (prune/stream s)
                     (#f          #f)
                     (`(,st . #f) (prune/goal st g))
                     ((pause st g1)
                      (match (prune/goal st g)
                        (#f           #f)
                        ((pause st g) (pause st (conj g1 g)))))
                     (s (match (prune/goal empty-state g)
                          (#f          #f)
                          ((pause _ _) (bind s g))))))
    ((pause st g)  (prune/goal st g))
    (`(,st . ,s)   `(,st . ,(prune/stream s)))
    (s             s)))

(define (prune/goal st g)
  (define (prune/term t) (walk* t (state-sub st)))
  (match g
    ((disj g1 g2)
     (match (prune/goal st g1)
       (#f (prune/goal st g2))
       ((pause st1 g1)
        (match (prune/goal st g2)
          (#f           (pause st1 g1))
          ((pause _ g2) (pause st (disj g1 g2)))))))
    ((conj g1 g2)
     (match (prune/goal st g1)
       (#f            #f)
       ((pause st g1) (match (prune/goal st g2)
                        (#f            #f)
                        ((pause st g2) (pause st (conj g1 g2)))))))
    ((relate thunk d) (pause st (relate thunk (prune/term d))))
    ((== t1 t2)
     (let ((t1 (prune/term t1)) (t2 (prune/term t2)))
       (match (unify t1 t2 st)
         (#f          #f)
         (st          (pause st (== t1 t2))))))
    ((=/= t1 t2)
     (let ((t1 (prune/term t1)) (t2 (prune/term t2)))
       (match (disunify t1 t2 st)
         (#f          #f)
         (st          (pause st (=/= t1 t2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transform into Disjunctive Normal Form.
(define (dnf/stream s)
  (define (push-pause st g)
    (match g
      ((disj g1 g2) (mplus (push-pause st g1) (push-pause st g2)))
      (g            (pause st g))))
  (match s
    ((bind s g)
     (let loop1 ((s (dnf/stream s)) (g (dnf/goal g)))
       (define (loop2 s g)
         (match g
           ((disj ga gb) (mplus (loop2 s ga) (loop2 s gb)))
           (g            (bind s g))))
       (match s
         ((mplus sa sb) (mplus (loop1 sa g) (loop1 sb g)))
         (`(,st . ,s)   (mplus (push-pause st g) (loop1 s g)))
         (s             (loop2 s g)))))
    ((pause st g)  (push-pause st (dnf/goal g)))
    ((mplus s1 s2) (mplus (dnf/stream s1) (dnf/stream s2)))
    (`(,st . ,s)   `(,st . ,(dnf/stream s)))
    (s             s)))

(define (dnf/goal g)
  (match g
    ((conj g1 g2)
     (let loop1 ((g1 (dnf/goal g1)) (g2 (dnf/goal g2)))
       (define (loop2 g1 g2)
         (match g2
           ((disj g2a g2b) (disj (loop2 g1 g2a) (loop2 g1 g2b)))
           (g2             (conj g1 g2))))
       (match g1
         ((disj g1a g1b) (disj (loop1 g1a g2) (loop1 g1b g2)))
         (g1             (loop2 g1 g2)))))
    ((disj g1 g2) (disj (dnf/goal g1) (dnf/goal g2)))
    (g            g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty formatting
(define (strip/stream s)
  (match s
    ((mplus s1 s2) (mplus (strip/stream s1) (strip/stream s2)))
    ((bind s g)    (bind  (strip/stream s)  g))
    ((pause st g)  (pause (strip/state st)  g))
    (`(,st . ,s)   `(,(strip/state st) . ,(strip/stream s)))
    (#f            #f)))

(define (strip/state st)
  (state `((,initial-var . ,(walk* initial-var (state-sub st))))))

(define (pretty/state st)
  `(state ,(map (lambda (vt) `(== ,(car vt) ,(walk* (car vt) (state-sub st))))
                (state-sub st))))

(define (pretty/stream s)
  (match s
    ((mplus s1 s2) `(mplus ,(pretty/stream s1) ,(pretty/stream s2)))
    ((bind s g)    `(bind  ,(pretty/stream s)  ,(pretty/goal empty-state g)))
    ((pause st g)  `(pause ,(pretty/state st)  ,(pretty/goal st g)))
    (`(,st . ,s)   `(cons  ,(pretty/state st)  ,(pretty/stream s)))
    (#f            #f)))

(define (pretty/goal st g)
  (define (pretty/term t) (walk* t (state-sub st)))
  (match g
    ((disj g1 g2)     `(disj ,(pretty/goal st g1) ,(pretty/goal st g2)))
    ((conj g1 g2)     `(conj ,(pretty/goal st g1) ,(pretty/goal st g2)))
    ((== t1 t2)       `(== ,(pretty/term t1) ,(pretty/term t2)))
    ((relate thunk d) `(relate ,(pretty/term (cdr d))))))

;; Parallel step
(define (parallel-step-simple s)
  (define (parallel-start st g)
    (match g
      ((disj g1 g2)     (parallel-step-simple (mplus (pause st g1)
                                                     (pause st g2))))
      ((conj g1 g2)     (parallel-step-simple (bind (pause st g1) g2)))
      ((relate thunk _) (pause st (thunk)))
      ((== t1 t2)       (unify t1 t2 st))))
  (define (parallel-expand g)
    (let loop ((g g))
      (match g
        ((conj g1 g2)     (conj (loop g1) (loop g2)))
        ((relate thunk _) (thunk))
        (_                g))))
  (match s
    ((mplus s1 s2)
     (let ((s1 (if (mature? s1) s1 (parallel-step-simple s1))))
       (cond ((not s1)   s2)
             ((pair? s1) (cons (car s1) (mplus s2 (cdr s1))))
             (else       (mplus s2 s1)))))
    ((bind s g)
     (let ((s (if (mature? s) s (parallel-step-simple s))))
       (cond ((not s)   #f)
             ((pair? s) (parallel-step-simple (mplus (pause (car s) g)
                                                     (bind (cdr s) g)))

                        )
             (else      (bind s (parallel-expand g))))))
    ((pause st g) (parallel-start st g))
    (_            s)))

(define (parallel-step s)
  (parallel-step-simple (prune/stream (dnf/stream s))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step-parameterized miniKanren run interface
(define (mature/step step s)
  (if (mature? s) s (mature/step step (step s))))
(define (stream-take/step step n s)
  (if (eqv? 0 n) '()
      (let ((s (mature/step step s)))
        (if (pair? s)
            (cons (car s) (stream-take/step step (and n (- n 1)) (cdr s)))
            '()))))

(define (simplify s)
  (prune/stream (dnf/stream s)))
(define-syntax run/step-simplify
  (syntax-rules ()
    ((_ step n body ...) (map reify/initial-var (stream-take/step
                                                 (lambda (s) (simplify (step s))) n (simplify (query body ...)))))))
(define-syntax run/step
  (syntax-rules ()
    ((_ step n body ...) (map reify/initial-var (stream-take/step
                                                 step n (query body ...))))))
(define-syntax run*/step
  (syntax-rules () ((_ step body ...) (run/step step #f body ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive query exploration
(define (pprint v)        (pretty-print v (current-output-port) 1))
(define (pprint/string v) (with-output-to-string (lambda () (pprint v))))
(define (pprint/margin margin prefix v)
  (define indent (string-append "\n" margin
                                (make-string (string-length prefix) #\space)))
  (define body (string-replace (string-trim (pprint/string v)) "\n" indent))
  (displayln (string-append margin prefix body)))

(define (stream->choices s)
  (let loop ((s (prune/stream (dnf/stream s))))
    (match s
      ((mplus s1 s2) (append (loop s1) (loop s2)))
      (#f            '())
      (`(,st . ,s)   (cons st (loop s)))
      (s             (list s)))))

(define (walked-term t st) (walk* t (state-sub st)))

(define (goal->constraints st g)
  ; takes a state and a goal and returns a list of Constraints
  ; which are just printable representations of goals
  (match g
    ((conj g1 g2) (append (goal->constraints st g1) (goal->constraints st g2)))
    ((relate _ d) (list (walked-term (cdr d) st)))
    ((=/= t1 t2)  `(,(list '=/= (walked-term t1 st) (walked-term t2 st))))
    (_            '())))  ;; == information has already been added to st.

(define margin "| ") ; define margin
(define (pp prefix v) (pprint/margin margin prefix v)) ; pretty-print with margin
(define (pp/qvars qvars vs) ; pretty-print the query variables
  (define (qv-prefix qv) (string-append " " (symbol->string qv) " = "))
  (define qv-prefixes (and qvars (map qv-prefix qvars)))
  (if qv-prefixes
      (for-each (lambda (prefix v) (pp prefix v)) qv-prefixes vs)
      (for-each (lambda (v) (pp " " v)) vs)))
(define (print-choice qvars s) ; Print the variables and constraints of a choice
  (match s
    ((pause st g)
      (pp/qvars qvars (walked-term initial-var st)) ; Print query variables
      (define cxs (walked-term (goal->constraints st g) st))
      (unless (null? cxs)
        (displayln (string-append margin " Constraints:"))
        (for-each (lambda (v) (pp " * " v)) cxs))
      (when (null? cxs)
        (displayln (string-append margin " No constraints"))))))

(define (previous-choice undo)
  ; undo -- current state of the program
  (and (pair? undo)
        (let* ((i.s (car undo)) (i (car i.s)) (s (cdr i.s)))
          (list-ref (dropf s state?) (- i 1)))))

(define (explore/stream step qvars s)
  ; step -- a procedure that takes a stream and returns a new stream
  ; qvars -- a list of query variables
  ; s -- a query
  (let loop ((s (stream->choices s)) ;; s is a list of choices
            (undo '())) ;; undo is a list of (depth . choice/result) pairs
    (define results (takef s state?))
    (define choices (dropf s state?))
    (printf "\n~s\n" s)
    (display "\n========================================")
    (displayln "========================================")
    (unless (= (length results) 0)
      (printf "Number of results: ~a\n" (length results))
      (for-each (lambda (st)
                  (pp/qvars qvars (walked-term initial-var st))
                  (newline))
                results))
    (when (and (previous-choice undo) (null? results))
      (printf "Previous Choice:\n")
      (print-choice qvars (previous-choice undo))
      (newline))
    (printf "Current Depth: ~a\n" (length undo))
    (if (= 0 (length choices))
        (if (= (length results) 0)
            (printf "Choice FAILED!  Undo to continue.\n")
            (printf "No more choices available.  Undo to continue.\n"))
        (printf "Number of Choices: ~a\n" (length choices)))
    (for-each (lambda (i s)
                (printf (string-append "\n" margin "Choice ~s:\n") (+ i 1))
                (print-choice qvars s))
              (range (length choices)) choices)
    (printf "\n[h]elp, [u]ndo, or choice number> \n")
    (define (invalid)
      (displayln "\nInvalid command or choice number.\nHit enter to continue.")
      (read-line) (read-line)
      (loop s undo))
    (define i (read))
    (cond ((eof-object? i) (newline))
          ((or (eq? i 'h) (eq? i 'help))
           (displayln
            (string-append "\nType either the letter 'u' or the"
                           " number following one of the listed choices."
                           "\nHit enter to continue."))
           (read-line) (read-line)
           (loop s undo))
          ((and (or (eq? i 'u) (eq? i 'undo)) (pair? undo))
           (loop (cdar undo) (cdr undo)))
          ((and (integer? i) (<= 1 i) (<= i (length choices)))
           (loop (stream->choices (step (list-ref choices (- i 1))))
                 (cons (cons i s) undo)))
          (else (invalid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define explore state to maintain the data held at every step
;; when exploring
(struct explore-state (history choices results) #:prefab)
(define (explore-state/can-undo? exp-state) (pair? (explore-state->history exp-state)))
(define (explore-state/make-choice exp-state choice)
  (match (exp-state)
    [(explore-state history _ _)
     (let* ([expanded-stream (step (list-ref choices choice))]
            [expanded-choices (stream->choices expnded-stream)]
            [choices (dropf expanded-choices state?)]
            [results (takef expanded-choices state?)])
       (explore-state (cons exp-state history) choices results)]))
(define (explore-state/make-choices exp-state choices)
  (match choices
    ['() exp-state]
    [(cons c hoices)
     (explore-state/make-choices (explore-state/make-choice exp-state c) hoices)]))

(define (pp/results qvars results)
  (unless (= (length results) 0)
    (printf "Number of results: ~a\n" (length results))
    (for-each (lambda (st)
                (pp/qvars qvars (walked-term initial-var st))
                (newline))
              results)))
(define (pp/choices qvars choices)
  (if (= 0 (length choices))
    (printf "No more choices available. Undo to continue.\n")
    (printf "Number of Choices: ~a\n" (length choices))))
    (for-each (lambda (i s)
                (printf (string-append "\n" margin "Choice ~s:\n") (+ i 1))
                (print-choice qvars s))
              (range (length choices)) choices)
(define (pp/explore-state exp-state qvars)
  (match exp-state
    [(explore-state history choices results)
     (display "\n========================================")
     (displayln "========================================")
     (pp/results qvars results)
     (when (and (previous-choice undo) (null? results))
       (printf "Previous Choice:\n")
       (print-choice qvars (previous-choice undo))
       (newline))
     (printf "Current Depth: ~a\n" (length undo))
     (pp/choices qvars choices)
     ]))

(define (explore-state/stream step qvars strm)
  (let* ([s (stream->choices strm)]
         [choices (dropf s state?)]
         [results (takef s state?)])
    (let loop ((exp-state (explore-state '() choices results)))
      (pp/explore-state exp-state)
      (define (invalid)
        (displayln "\nInvalid command or choice number.")
        (loop exp-state))
      (printf "\n[h]elp, [u]ndo, or choice number> \n")
      (define i (read))
      (cond
        [(eof-object? i) (newline)]
        [(or (eq? i 'h) (eq? i 'help))
         (displayln
            (string-append "\nType either the letter 'u' or the"
                           " number following one of the listed choices."
                           "\nHit enter to continue."))
         (read-line)
         (read-line)
         (loop exp-state)]
        [(and (or (eq? i 'u) (eq? i 'undo)) (pair? undo))
         (loop (explore-state/undo exp-state))]
        [(and (integer? i) (<= 1 i) (<= i (length (explore-state->choices exp-state))))
         (loop (explore-state/make-choice exp-state (- i 1)))]
        [else (invalid)]
        ))))

(define-syntax explore
  (syntax-rules (query)
    ((_ step (query (qvars ...) body ...))
     (begin (printf "Using step procedure: ~s\nExploring query:\n~s\n"
                    'step '(query (qvars ...) body ...))
            (explore/stream step '(qvars ...) (query (qvars ...) body ...))))
    ((_ step stream) (explore/stream step #f stream))))
