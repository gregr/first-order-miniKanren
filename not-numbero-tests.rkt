(display "\nRunning not-numbero tests")
(newline)

(test 'not-numbero-0
  (run* (x) (not-numbero x))
  '(#s(Ans (_.0) ((not-num _.0)))))

(test 'not-numbero-1
  (run* (x) (not-numbero 1))
  '())

(test 'not-numbero-2-a
  (run* (x) (not-numbero x) (== x "hello"))
  '(("hello")))

(test 'not-numbero-2-b
  (run* (x) (== x "hello") (not-numbero x))
  '(("hello")))

(test 'not-numbero-3-a
  (run* (x) (not-numbero x) (== x 3))
  '())

(test 'not-numbero-3-b
  (run* (x) (== x 3) (not-numbero x))
  '())

(test 'not-numbero-4-a
  (run* (x) (fresh (q) (not-numbero x) (== q x) (== q 4)))
  '())

(test 'not-numbero-4-b
  (run* (x) (fresh (q) (not-numbero x) (== q 4) (== q x)))
  '())

(test 'not-numbero-4-c
  (run* (x) (fresh (q) (== q x) (not-numbero x) (== q 4)))
  '())

(test 'not-numbero-5-a
  (run* (x) (fresh (q) (not-numbero x) (== q x) (== q 'a)))
  '((a)))

(test 'not-numbero-5-b
  (run* (x) (fresh (q) (not-numbero x) (== q 'a) (== q x)))
  '((a)))

(test 'not-numbero-5-c
  (run* (x) (fresh (q) (== q x) (not-numbero x) (== q 'a)))
  '((a)))

(test 'not-numbero-6-a
  (run* (x) (fresh (q) (not-numbero x) (== q x) (not-numbero q)))
  '(#s(Ans (_.0) ((not-num _.0)))))

(test 'not-numbero-6-b
  (run* (x) (fresh (q) (not-numbero x) (not-numbero q) (== q x)))
  '(#s(Ans (_.0) ((not-num _.0)))))

(test 'not-numbero-6-c
  (run* (x) (fresh (q) (== q x) (not-numbero x) (not-numbero q)))
  '(#s(Ans (_.0) ((not-num _.0)))))

(test 'not-numbero-7-a
  (run* (x) (not-numbero x) (=/= x 7))
  '(#s(Ans (_.0) ((not-num _.0)))))

(test 'not-numbero-7-b
  (run* (x) (=/= x 7) (not-numbero x))
  '(#s(Ans (_.0) ((not-num _.0)))))

(test 'not-numbero-8-a
  (run* (x) (not-numbero x) (=/= x 'a))
  '(#s(Ans (_.0) ((=/= ((_.0 a))) (not-num _.0)))))

(test 'not-numbero-8-b
  (run* (x) (=/= x 'a) (not-numbero x))
  '(#s(Ans (_.0) ((=/= ((_.0 a))) (not-num _.0)))))

(test 'not-numbero-9a
  (run* (x)
    (fresh (p q)
      (not-numbero p)
      (not-numbero q)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0) (not-num _.1)))))

(test 'not-numbero-9b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (not-numbero p)
      (not-numbero q)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0) (not-num _.1)))))

(test 'not-numbero-10a
  (run* (x)
    (fresh (p q)
      (not-numbero p)
      (not-numbero p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0)))))

(test 'not-numbero-10b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (not-numbero p)
      (not-numbero p)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0)))))

(test 'not-numbero-10c
  (run* (x)
    (fresh (p q)
      (not-numbero p)
      (== `(,p ,q) x)
      (not-numbero p)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0)))))

(test 'not-numbero-11a
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-numbero p)
      (not-numbero q)))
  '((_.0)))

(test 'not-numbero-11b
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-numbero p)
      (not-numbero q)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((=/= ((_.0 _.2) (_.1 _.3))) (not-num _.0) (not-num _.1)))))

(test 'not-numbero-11c
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-numbero p)
      (not-numbero r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((=/= ((_.0 _.2) (_.1 _.3))) (not-num _.0) (not-num _.2)))))

(test 'not-numbero-11d
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-numbero p)
      (not-numbero r)
      (== p r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.0 _.2)) ((=/= ((_.1 _.2))) (not-num _.0)))))

(test 'not-numbero-12a
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `(12 . 13))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 12) (_.1 13)))))))

(test 'not-numbero-12b
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `(12 . 13))
      (not-numbero p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0)))))

(test 'not-numbero-12c
  (run* (x)
    (fresh (p q)
      (not-numbero p)
      (=/= `(,p . ,q) `(12 . 13))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0)))))

(test 'not-numbero-12d
  (run* (x)
    (fresh (p q)
      (not-numbero p)
      (=/= `(12 . 13) `(,p . ,q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0)))))

(test 'not-numbero-12e
  (run* (x)
    (fresh (p q)
      (not-numbero p)
      (=/= `(12 . ,q) `(,p . 13))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0)))))

(test 'not-numbero-12f
  (run* (x)
    (fresh (p q)
      (not-numbero p)
      (=/= `("wysi" . ,q) `(,p . 727))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 "wysi") (_.1 727))) (not-num _.0)))))

(test 'not-numbero-13a
  (run* (x) (not-numbero x) (conde ((numbero x)) ((== x "hello"))))
  '(("hello")))

(test 'not-numbero-13b
  (run* (x) (not-numbero x) (conde ((== x 'x)) ((== x "hello"))))
  '((x) ("hello")))
