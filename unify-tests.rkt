(display "\nRunning == tests")
(newline)

(test 'equality-0
  (run* (q) (== 1 1))
  '((_.0)))

(test 'equality-1
  (run* (q) (== 5 q))
  '((5)))

(test 'equality-2
  (run* (q) (== q 5))
  '((5)))

(test 'equality-3
  (run* (p q) (== p q))
  '((_.0 _.0)))

(test 'equality-4
  (run* (p q) (== (cons p 3) (cons 5 q)))
  '((5 3)))

(test 'equality-5
  (run* (p q r) (== p q) (== q r))
  '((_.0 _.0 _.0)))

(test 'equality-6
  (run* (q) (== q 'hello))
  '((hello)))

(test 'equality-7
  (run* (q) (== q "world"))
  '(("world")))

(test 'equality-with-conde-0
  (run* (q) (conde ((== 4 q)) ((== 1 q))))
  '((4) (1)))

(test 'equality-with-conde-1
  (run* (p q) (== 9 p) (conde ((== 7 q)) ((== 8 q))))
  '((9 7) (9 8)))

(test 'equality-with-conde-2
  (run* (p q r) (conde ((== p q)) ((== p r))))
  '((_.0 _.0 _.1) (_.0 _.1 _.0)))

(test 'equality-fail-0
  (run* (q) (== 3 90))
  '())

(test 'equality-fail-1
  (run* (q) (== q 41) (== q 64))
  '())

(test 'equality-fail-2
  (run* (q) (== 93 37) (== q 6))
  '())

(test 'equality-fresh-test-0
  (run* (a b) (fresh (c) (== a c) (== b c)))
  '((_.0 _.0)))

(test 'equality-pair-test-0
  (run* (a) (== a (cons 55 249)))
  '(((55 . 249))))

(test 'equality-pair-test-1
  (run* (a b c) (== c (cons a b)))
  '((_.0 _.1 (_.0 . _.1))))

; Faster-miniKanren tests
(test "1"
  (run 1 (q) (== 5 q))
  '((5)))

(test "2"
  (run* (q)
  (conde
    [(== 5 q)]
    [(== 6 q)]))
  '((5) (6)))

(test "3"
  (run* (q)
  (fresh (a d)
    (conde
      [(== 5 a)]
      [(== 6 d)])
    (== `(,a . ,d) q)))
  '(((5 . _.0)) ((_.0 . 6))))