(display "\nRunning distype-diseq tests")
(newline)

;; checks simplification
(test 'distype-diseq-0
  (run* (x) (not-numbero x) (=/= x 5))
  '(#s(Ans (_.0) ((not-num _.0))))
)

(test 'distype-diseq-1
  (run* (x) (not-symbolo x) (=/= x 5))
  '(#s(Ans (_.0) ((=/= ((_.0 5))) (not-sym _.0))))
)

(test 'distype-diseq-2
  (run* (x) (not-stringo x) (=/= x 5))
  '(#s(Ans (_.0) ((=/= ((_.0 5))) (not-str _.0))))
)

(test 'distype-diseq-3
  (run* (x) (not-numbero x) (=/= x 'a))
  '(#s(Ans (_.0) ((=/= ((_.0 a))) (not-num _.0))))
)

(test 'distype-diseq-4
  (run* (x) (not-symbolo x) (=/= x 'a))
  '(#s(Ans (_.0) ((not-sym _.0))))
)

(test 'distype-diseq-5
  (run* (x) (not-stringo x) (=/= x 'a))
  '(#s(Ans (_.0) ((=/= ((_.0 a))) (not-str _.0))))
)

(test 'distype-diseq-6
  (run* (x) (not-numbero x) (=/= x "test"))
  '(#s(Ans (_.0) ((=/= ((_.0 "test"))) (not-num _.0))))
)

(test 'distype-diseq-7
  (run* (x) (not-symbolo x) (=/= x "test"))
  '(#s(Ans (_.0) ((=/= ((_.0 "test"))) (not-sym _.0))))
)

(test 'distype-diseq-8
  (run* (x) (not-stringo x) (=/= x "test"))
  '(#s(Ans (_.0) ((not-str _.0))))
)

(test 'distype-diseq-9
  (run* (x) (not-stringo x) (not-numbero x) (not-symbolo x) (=/= x 5) (=/= x 'a) (=/= x "test"))
  '(#s(Ans (_.0) ((not-num _.0) (not-str _.0) (not-sym _.0))))
)

(test 'distype-diseq-10
  (run* (x) (fresh (a) (not-numbero x) (not-stringo a) (== x a)))
  '(#s(Ans (_.0) ((not-num _.0) (not-str _.0))))
)

(
  test 'distype-diseq-11
  (run* (x) (fresh (a b) (== x (cons a b)) (not-numbero a) (not-symbolo b)))
  '(#s(Ans ((_.0 . _.1)) ((not-num _.0) (not-sym _.1))))
)

;; test cons/list
(
  test 'distype-diseq-12
  (run* (x) (fresh (a b c) (== x (cons a b)) (not-numbero a) (not-symbolo b) (not-stringo c)))
  '(#s(Ans ((_.0 . _.1)) ((not-num _.0) (not-sym _.1))))
)

(
  test 'distype-diseq-13
  (run* (x) (fresh (a b c) (== x (list a b c)) (not-numbero a) (not-symbolo b) (not-stringo c)))
  '(#s(Ans ((_.0 _.1 _.2)) ((not-num _.0) (not-str _.2) (not-sym _.1))))
)

;; test order
(
  test 'distype-diseq-14
  (run* (x) (fresh (a b c) (not-stringo c) (not-numbero a) (== x (list a b c)) (not-symbolo b) ))
  '(#s(Ans ((_.0 _.1 _.2)) ((not-num _.0) (not-str _.2) (not-sym _.1))))
)

(
  test 'distype-diseq-15
  (run* (x) (fresh (a b c)  (not-symbolo b) (not-stringo c) (== x (list a b c)) (not-numbero a)))
  '(#s(Ans ((_.0 _.1 _.2)) ((not-num _.0) (not-str _.2) (not-sym _.1))))
)

(test 'distype-diseq-16 ;; from 6
  (run* (x) (=/= x "test") (not-numbero x))
  '(#s(Ans (_.0) ((=/= ((_.0 "test"))) (not-num _.0))))
)

(test 'distype-diseq-17 ;; from 7
  (run* (x) (=/= x "test") (not-symbolo x))
  '(#s(Ans (_.0) ((=/= ((_.0 "test"))) (not-sym _.0))))
)

;; many constraints
(test 'distype-diseq-18
  (run* (x) (fresh (a b c) (== x (list a b c)) (not-numbero a) (not-symbolo b) (not-stringo c) (=/= a "test") (=/= b 'a) (=/= c 5)))
  '(#s(Ans ((_.0 _.1 _.2)) ((=/= ((_.0 "test")) ((_.2 5))) (not-num _.0) (not-str _.2) (not-sym _.1))))
)

(test 'distype-diseq-19
  (run* (x) (fresh (a b c) (== x (list a b c)) (not-numbero a) (not-symbolo b) (not-stringo c) (=/= a "test") (=/= b 'a) (fresh (c2 c3) (== c (cons c2 c3)) (not-numbero c2) (not-symbolo c3))))
  '(#s(Ans ((_.0 _.1 (_.2 . _.3))) ((=/= ((_.0 "test"))) (not-num _.0) (not-num _.2) (not-sym _.1) (not-sym _.3))))
)