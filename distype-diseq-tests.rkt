(display "\nRunning distype-diseq tests")
(newline)

;; checks simplification
(test 'distype-diseq-0
  (run* (x) (!numbero x) (=/= x 5))
  '(#s(Ans (_.0) ((!num _.0))))
)

(test 'distype-diseq-1
  (run* (x) (!symbolo x) (=/= x 5))
  '(#s(Ans (_.0) ((!sym _.0) (=/= ((_.0 5))))))
)

(test 'distype-diseq-2
  (run* (x) (!stringo x) (=/= x 5))
  '(#s(Ans (_.0) ((!str _.0) (=/= ((_.0 5))))))
)

(test 'distype-diseq-3
  (run* (x) (!numbero x) (=/= x 'a))
  '(#s(Ans (_.0) ((!num _.0) (=/= ((_.0 a))))))
)

(test 'distype-diseq-4
  (run* (x) (!symbolo x) (=/= x 'a))
  '(#s(Ans (_.0) ((!sym _.0))))
)

(test 'distype-diseq-5
  (run* (x) (!stringo x) (=/= x 'a))
  '(#s(Ans (_.0) ((!str _.0) (=/= ((_.0 a))))))
)

(test 'distype-diseq-6
  (run* (x) (!numbero x) (=/= x "test"))
  '(#s(Ans (_.0) ((!num _.0) (=/= ((_.0 "test"))))))
)

(test 'distype-diseq-7
  (run* (x) (!symbolo x) (=/= x "test"))
  '(#s(Ans (_.0) ((!sym _.0) (=/= ((_.0 "test"))))))
)

(test 'distype-diseq-8
  (run* (x) (!stringo x) (=/= x "test"))
  '(#s(Ans (_.0) ((!str _.0))))
)

(test 'distype-diseq-9
  (run* (x) (!stringo x) (!numbero x) (!symbolo x) (=/= x 5) (=/= x 'a) (=/= x "test"))
  '(#s(Ans (_.0) ((!num _.0) (!str _.0) (!sym _.0))))
)

(test 'distype-diseq-10
  (run* (x) (fresh (a) (!numbero x) (!stringo a) (== x a)))
  '(#s(Ans (_.0) ((!num _.0) (!str _.0))))
)

(
  test 'distype-diseq-11
  (run* (x) (fresh (a b) (== x (cons a b)) (!numbero a) (!symbolo b)))
  '(#s(Ans ((_.0 . _.1)) ((!num _.0) (!sym _.1))))
)

;; test cons/list
(
  test 'distype-diseq-12
  (run* (x) (fresh (a b c) (== x (cons a b)) (!numbero a) (!symbolo b) (!stringo c)))
  '(#s(Ans ((_.0 . _.1)) ((!num _.0) (!sym _.1))))
)

(
  test 'distype-diseq-13
  (run* (x) (fresh (a b c) (== x (list a b c)) (!numbero a) (!symbolo b) (!stringo c)))
  '(#s(Ans ((_.0 _.1 _.2)) ((!num _.0) (!str _.2) (!sym _.1))))
)

;; test order
(
  test 'distype-diseq-14
  (run* (x) (fresh (a b c) (!stringo c) (!numbero a) (== x (list a b c)) (!symbolo b) ))
  '(#s(Ans ((_.0 _.1 _.2)) ((!num _.0) (!str _.2) (!sym _.1))))
)

(
  test 'distype-diseq-15
  (run* (x) (fresh (a b c)  (!symbolo b) (!stringo c) (== x (list a b c)) (!numbero a)))
  '(#s(Ans ((_.0 _.1 _.2)) ((!num _.0) (!str _.2) (!sym _.1))))
)

(test 'distype-diseq-16 ;; from 6
  (run* (x) (=/= x "test") (!numbero x))
  '(#s(Ans (_.0) ((!num _.0) (=/= ((_.0 "test"))))))
)

(test 'distype-diseq-17 ;; from 7
  (run* (x) (=/= x "test") (!symbolo x))
  '(#s(Ans (_.0) ((!sym _.0) (=/= ((_.0 "test"))))))
)

;; many constraints
(test 'distype-diseq-18
  (run* (x) (fresh (a b c) (== x (list a b c)) (!numbero a) (!symbolo b) (!stringo c) (=/= a "test") (=/= b 'a) (=/= c 5)))
  '(#s(Ans ((_.0 _.1 _.2)) ((!num _.0) (!str _.2) (!sym _.1) (=/= ((_.0 "test")) ((_.2 5))))))
)

(test 'distype-diseq-19
  (run* (x) (fresh (a b c) (== x (list a b c)) (!numbero a) (!symbolo b) (!stringo c) (=/= a "test") (=/= b 'a) (fresh (c2 c3) (== c (cons c2 c3)) (!numbero c2) (!symbolo c3))))
  '(#s(Ans ((_.0 _.1 (_.2 . _.3))) ((!num _.0) (!num _.2) (!sym _.1) (!sym _.3) (=/= ((_.0 "test"))))))
)