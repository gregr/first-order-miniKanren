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

;; conjunction of disjunctions
(test 'distype-diseq-20
      (run* (x)
            (conde ((not-stringo x)) ((not-numbero x)))
            (conde ((== x "x")) ((== x 5))))
      '((5) ("x")))

(test 'distype-diseq-21
      (run* (x)
            (conde ((not-stringo x)) ((not-numbero x)))
            (conde ((=/= x "x")) ((=/= x 5))))
      '(#s(Ans (_.0) ((not-str _.0)))
        #s(Ans (_.0) ((=/= ((_.0 5))) (not-str _.0)))
        #s(Ans (_.0) ((=/= ((_.0 "x"))) (not-num _.0)))
        #s(Ans (_.0) ((not-num _.0)))))

(test 'distype-diseq-22a
      (run* (x)
            (conde ((not-stringo x)) ((not-numbero x)))
            (conde ((not-symbolo x)) ((numbero x))))
      '(#s(Ans (_.0) ((not-str _.0) (not-sym _.0)))
        #s(Ans (_.0) ((num _.0)))
        #s(Ans (_.0) ((not-num _.0) (not-sym _.0)))))

(test 'distype-diseq-22b
      (run* (x)
            (conde ((not-stringo x)) ((not-numbero x)))
            (conde ((not-symbolo x)) ((numbero x)))
            (stringo x))
      '(#s(Ans (_.0) ((str _.0)))))

(test 'distype-diseq-22c
      (run* (x)
            (conde ((not-stringo x)) ((not-numbero x)) ((not-symbolo x)))
            (conde ((not-symbolo x)) ((numbero x))))
      '(#s(Ans (_.0) ((not-str _.0) (not-sym _.0)))
        #s(Ans (_.0) ((num _.0)))
        #s(Ans (_.0) ((not-num _.0) (not-sym _.0)))
        #s(Ans (_.0) ((not-sym _.0)))
        #s(Ans (_.0) ((num _.0)))))

(test 'distype-diseq-22d
      (run* (x)
            (conde ((not-stringo x)) ((not-numbero x)) ((not-symbolo x)))
            (conde ((not-symbolo x)) ((numbero x)))
            (stringo x))
      '(#s(Ans (_.0) ((str _.0)))
        #s(Ans (_.0) ((str _.0)))))

(test 'distype-diseq-23a
      (run* (x) (conde ((not-numbero x) (== x 26))
                       ((not-stringo x) (== x "x"))
                       ((not-symbolo x) (== x 'x))))
      '())

(test 'distype-diseq-23b
      (run* (x) (conde ((not-numbero x) (=/= x 26))
                       ((not-stringo x) (== x "x"))
                       ((not-symbolo x) (== x 'x))))
      '(#s(Ans (_.0) ((not-num _.0)))))

(test 'distype-diseq-23c
      (run* (x) (conde ((not-numbero x) (=/= x 26))
                       ((not-stringo x) (=/= x "x"))
                       ((not-symbolo x) (== x 'x))))
      '(#s(Ans (_.0) ((not-num _.0)))
        #s(Ans (_.0) ((not-str _.0)))))

(test 'distype-diseq-23d
      (run* (x) (conde ((not-numbero x) (=/= x 26))
                       ((not-stringo x) (=/= x "x"))
                       ((not-symbolo x) (=/= x 'x))))
      '(#s(Ans (_.0) ((not-num _.0)))
        #s(Ans (_.0) ((not-str _.0)))
        #s(Ans (_.0) ((not-sym _.0)))))

(test 'distype-diseq-24
      (run* (x) (fresh (y)
                       (conde ((not-numbero y)) ((not-stringo x)))
                       (conde ((numbero y)) ((stringo x)))))
      '(#s(Ans (_.0) ((not-str _.0)))
        #s(Ans (_.0) ((str _.0)))))

(test 'distype-diseq-25
      (run* (x) (fresh (y)
                       (conde ((== x 2)) ((== y 5)))
                       (conde ((stringo y)) ((symbolo y)))))
      '((2)
        (2)))

(test 'distype-diseq-26a
      (run* (x) (fresh (y)
                       (conde ((== y 2)) ((== y 6)))
                       (conde ((not-stringo y)) ((not-symbolo y)))
                       (conde ((=/= y "hello")) ((=/= y 'x)))))
      '((_.0)
        (_.0)
        (_.0)
        (_.0)
        (_.0)
        (_.0)
        (_.0)
        (_.0)))

(test 'distype-diseq-26b
      (run* (x) (fresh (y)
                       (conde ((== y 2)) ((== y "hello")))
                       (conde ((not-stringo y)) ((not-symbolo y)))
                       (conde ((=/= y "hello")) ((=/= y 'x)))))
      '((_.0)
        (_.0)
        (_.0)
        (_.0)
        (_.0)))

(test 'distype-diseq-26c
      (run* (x) (fresh (y)
                       (conde ((== y 2)) ((== y "hello")))
                       (conde ((not-stringo y)) ((not-symbolo y)))
                       (conde ((=/= y "hello")) ((=/= y 'x)))
                       (=/= y 2)))
      '((_.0)))

(test 'distype-diseq-26d
      (run* (x) (fresh (y)
                       (conde ((== y 2)) ((== y "hello")) ((== x 1)))
                       (conde ((not-stringo y)) ((not-symbolo y)) ((== x 2)))
                       (conde ((=/= y "hello")) ((=/= y 'x)) ((== x 3)))
                       (=/= y 2)))
      '((_.0)
        (1)
        (3)
        (1)
        (2)
        (1)
        (1)))

(test 'distype-diseq-26e
      (run* (x) (fresh (y)
                       (conde ((== y 2)) ((== y "hello")) ((== x 1)))
                       (conde ((not-stringo y)) ((not-symbolo y)) ((== x 2)))
                       (conde ((=/= y "hello")) ((=/= y 'x)) ((== x 3)))
                       (conde ((=/= y 2)) ((== x 4)))))
      '((4)
        (4)
        (4)
        (4)
        (_.0)
        (1)
        (4)
        (3)
        (1)
        (2)
        (1)
        (1)))

(test 'distype-diseq-27
      (run* (x) (fresh (y)
                       (conde ((not-stringo y)) ((not-numbero x)))
                       (conde ((== y 'z)) ((== y 27)))
                       (=/= x y)))
      '(#s(Ans (_.0) ((=/= ((_.0 z)))))
        #s(Ans (_.0) ((=/= ((_.0 z))) (not-num _.0)))
        #s(Ans (_.0) ((=/= ((_.0 27)))))
        #s(Ans (_.0) ((not-num _.0)))))

(test 'distype-diseq-28a
      (run* (x y)
            (=/= x y)
            (not-stringo x)
            (stringo y))
      '(#s(Ans (_.0 _.1) ((not-str _.0) (str _.1)))))

(test 'distype-diseq-28b
      (run* (x y)
            (not-stringo x)
            (=/= x y)
            (stringo y))
      '(#s(Ans (_.0 _.1) ((not-str _.0) (str _.1)))))

(test 'distype-diseq-28c
      (run* (x y)
            (not-stringo x)
            (stringo y)
            (=/= x y))
      '(#s(Ans (_.0 _.1) ((not-str _.0) (str _.1)))))

(test 'distype-diseq-28d
      (run* (x y)
            (=/= x y)
            (== x 5)
            (stringo y))
      '(#s(Ans (5 _.0) ((str _.0)))))

(test 'distype-diseq-28e
      (run* (x y)
            (== x 5)
            (=/= x y)
            (stringo y))
      '(#s(Ans (5 _.0) ((str _.0)))))

(test 'distype-diseq-28f
      (run* (x y)
            (== x 5)
            (stringo y)
            (=/= x y))
      '(#s(Ans (5 _.0) ((str _.0)))))
