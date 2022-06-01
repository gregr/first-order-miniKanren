(display "\nRunning not-symbolo tests")
(newline)

(test 'not-symbolo-0
  (run* (x) (not-symbolo x))
  '(#s(Ans (_.0) ((not-sym _.0)))))

(test 'not-symbolo-1
  (run* (x) (not-symbolo 'x))
  '())

(test 'not-symbolo-2-a
  (run* (x) (not-symbolo x) (== x "hello"))
  '(("hello")))

(test 'not-symbolo-2-b
  (run* (x) (== x "hello") (not-symbolo x))
  '(("hello")))

(test 'not-symbolo-3-a
  (run* (x) (not-symbolo x) (== x 'x))
  '())

(test 'not-symbolo-3-b
  (run* (x) (== x 'x) (not-symbolo x))
  '())

(test 'not-symbolo-4-a
  (run* (x) (fresh (q) (not-symbolo x) (== q x) (== q 'q)))
  '())

(test 'not-symbolo-4-b
  (run* (x) (fresh (q) (not-symbolo x) (== q 'q) (== q x)))
  '())

(test 'not-symbolo-4-c
  (run* (x) (fresh (q) (== q x) (not-symbolo x) (== q 'q)))
  '())

(test 'not-symbolo-5-a
  (run* (x) (fresh (q) (not-symbolo x) (== q x) (== q "a")))
  '(("a")))

(test 'not-symbolo-5-b
  (run* (x) (fresh (q) (not-symbolo x) (== q "a") (== q x)))
  '(("a")))

(test 'not-symbolo-5-c
  (run* (x) (fresh (q) (== q x) (not-symbolo x) (== q "a")))
  '(("a")))

(test 'not-symbolo-6-a
  (run* (x) (fresh (q) (not-symbolo x) (== q x) (not-symbolo q)))
  '(#s(Ans (_.0) ((not-sym _.0)))))

(test 'not-symbolo-6-b
  (run* (x) (fresh (q) (not-symbolo x) (not-symbolo q) (== q x)))
  '(#s(Ans (_.0) ((not-sym _.0)))))

(test 'not-symbolo-6-c
  (run* (x) (fresh (q) (== q x) (not-symbolo x) (not-symbolo q)))
  '(#s(Ans (_.0) ((not-sym _.0)))))

(test 'not-symbolo-7-a
  (run* (x) (not-symbolo x) (=/= x 'x))
  '(#s(Ans (_.0) ((not-sym _.0)))))

(test 'not-symbolo-7-b
  (run* (x) (=/= x 'x) (not-symbolo x))
  '(#s(Ans (_.0) ((not-sym _.0)))))

(test 'not-symbolo-8-a
  (run* (x) (not-symbolo x) (=/= x 8))
  '(#s(Ans (_.0) ((=/= ((_.0 8))) (not-sym _.0)))))

(test 'not-symbolo-8-b
  (run* (x) (=/= x 8) (not-symbolo x))
  '(#s(Ans (_.0) ((=/= ((_.0 8))) (not-sym _.0)))))

(test 'not-symbolo-9a
  (run* (x)
    (fresh (p q)
      (not-symbolo p)
      (not-symbolo q)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.0) (not-sym _.1)))))

(test 'not-symbolo-9b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (not-symbolo p)
      (not-symbolo q)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.0) (not-sym _.1)))))

(test 'not-symbolo-10a
  (run* (x)
    (fresh (p q)
      (not-symbolo p)
      (not-symbolo p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.0)))))

(test 'not-symbolo-10b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (not-symbolo p)
      (not-symbolo p)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.0)))))

(test 'not-symbolo-10c
  (run* (x)
    (fresh (p q)
      (not-symbolo p)
      (== `(,p ,q) x)
      (not-symbolo p)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.0)))))

(test 'not-symbolo-11a
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-symbolo p)
      (not-symbolo q)))
  '((_.0)))

(test 'not-symbolo-11b
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-symbolo p)
      (not-symbolo q)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((=/= ((_.0 _.2) (_.1 _.3))) (not-sym _.0) (not-sym _.1)))))

(test 'not-symbolo-11c
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-symbolo p)
      (not-symbolo r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((=/= ((_.0 _.2) (_.1 _.3))) (not-sym _.0) (not-sym _.2)))))

(test 'not-symbolo-11d
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-symbolo p)
      (not-symbolo r)
      (== p r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.0 _.2)) ((=/= ((_.1 _.2))) (not-sym _.0)))))

(test 'not-symbolo-12a
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `(p . q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 p) (_.1 q)))))))

(test 'not-symbolo-12b
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `(p . q))
      (not-symbolo p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.0)))))

(test 'not-symbolo-12c
  (run* (x)
    (fresh (p q)
      (not-symbolo p)
      (=/= `(,p . ,q) `(p . q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.0)))))

(test 'not-symbolo-12d
  (run* (x)
    (fresh (p q)
      (not-symbolo p)
      (=/= `(p . q) `(,p . ,q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.0)))))

(test 'not-symbolo-12e
  (run* (x)
    (fresh (p q)
      (not-symbolo p)
      (=/= `(p . ,q) `(,p . q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.0)))))

(test 'not-symbolo-12f
  (run* (x)
    (fresh (p q)
      (not-symbolo p)
      (=/= `("q" . ,q) `(,p . 'p))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 "q") (_.1 'p))) (not-sym _.0)))))

(test 'not-symbolo-13a
  (run* (x) (not-symbolo x) (conde ((symbolo x)) ((== x "hello"))))
  '(("hello")))

(test 'not-symbolo-13b
  (run* (x) (not-symbolo x) (conde ((== x 13)) ((== x "hello"))))
  '((13) ("hello")))
