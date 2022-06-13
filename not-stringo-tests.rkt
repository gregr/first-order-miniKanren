(display "\nRunning not-stringo tests")
(newline)

(test 'not-stringo-0
  (run* (x) (not-stringo x))
  '(#s(Ans (_.0) ((not-str _.0)))))

(test 'not-stringo-1
  (run* (x) (not-stringo "f"))
  '())

(test 'not-stringo-2-a
  (run* (x) (not-stringo x) (== x 5))
  '((5)))

(test 'not-stringo-2-b
  (run* (x) (== x 5) (not-stringo x))
  '((5)))

(test 'not-stringo-3-a
  (run* (x) (not-stringo x) (== x "foo"))
  '())

(test 'not-stringo-3-b
  (run* (x) (== x "foo") (not-stringo x))
  '())

(test 'not-stringo-4-a
  (run* (x) (fresh (q) (not-stringo x) (== q x) (== q "bar")))
  '())

(test 'not-stringo-4-b
  (run* (x) (fresh (q) (not-stringo x) (== q "bar") (== q x)))
  '())

(test 'not-stringo-4-c
  (run* (x) (fresh (q) (== q x) (not-stringo x) (== q "bar")))
  '())

(test 'not-stringo-5-a
  (run* (x) (fresh (q) (not-stringo x) (== q x) (== q 'a)))
  '((a)))

(test 'not-stringo-5-b
  (run* (x) (fresh (q) (not-stringo x) (== q 'a) (== q x)))
  '((a)))

(test 'not-stringo-5-c
  (run* (x) (fresh (q) (== q x) (not-stringo x) (== q 'a)))
  '((a)))

(test 'not-stringo-6-a
  (run* (x) (fresh (q) (not-stringo x) (== q x) (not-stringo q)))
  '(#s(Ans (_.0) ((not-str _.0)))))

(test 'not-stringo-6-b
  (run* (x) (fresh (q) (not-stringo x) (not-stringo q) (== q x)))
  '(#s(Ans (_.0) ((not-str _.0)))))

(test 'not-stringo-6-c
  (run* (x) (fresh (q) (== q x) (not-stringo x) (not-stringo q)))
  '(#s(Ans (_.0) ((not-str _.0)))))

(test 'not-stringo-7-a
  (run* (x) (not-stringo x) (=/= x "hello"))
  '(#s(Ans (_.0) ((not-str _.0)))))

(test 'not-stringo-7-b
  (run* (x) (=/= x "hello") (not-stringo x))
  '(#s(Ans (_.0) ((not-str _.0)))))

(test 'not-stringo-8-a
  (run* (x) (not-stringo x) (=/= x 5))
  '(#s(Ans (_.0) ((=/= ((_.0 5))) (not-str _.0)))))

(test 'not-stringo-8-b
  (run* (x) (=/= x 5) (not-stringo x))
  '(#s(Ans (_.0) ((=/= ((_.0 5))) (not-str _.0)))))

(test 'not-stringo-9a
  (run* (x)
    (fresh (p q)
      (not-stringo p)
      (not-stringo q)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-str _.0) (not-str _.1)))))

(test 'not-stringo-9b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (not-stringo p)
      (not-stringo q)))
  '(#s(Ans ((_.0 _.1)) ((not-str _.0) (not-str _.1)))))

(test 'not-stringo-10a
  (run* (x)
    (fresh (p q)
      (not-stringo p)
      (not-stringo p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-str _.0)))))

(test 'not-stringo-10b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (not-stringo p)
      (not-stringo p)))
  '(#s(Ans ((_.0 _.1)) ((not-str _.0)))))

(test 'not-stringo-10c
  (run* (x)
    (fresh (p q)
      (not-stringo p)
      (== `(,p ,q) x)
      (not-stringo p)))
  '(#s(Ans ((_.0 _.1)) ((not-str _.0)))))

(test 'not-stringo-11a
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-stringo p)
      (not-stringo q)))
  '((_.0)))

(test 'not-stringo-11b
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-stringo p)
      (not-stringo q)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((=/= ((_.0 _.2) (_.1 _.3))) (not-str _.0) (not-str _.1)))))

(test 'not-stringo-11c
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-stringo p)
      (not-stringo r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((=/= ((_.0 _.2) (_.1 _.3))) (not-str _.0) (not-str _.2)))))

(test 'not-stringo-11d
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (not-stringo p)
      (not-stringo r)
      (== p r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.0 _.2)) ((=/= ((_.1 _.2))) (not-str _.0)))))

(test 'not-stringo-12a
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `("hello" . "world"))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 "hello") (_.1 "world")))))))

(test 'not-stringo-12b
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `("hello" . "world"))
      (not-stringo p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-str _.0)))))

(test 'not-stringo-12c
  (run* (x)
    (fresh (p q)
      (not-stringo p)
      (=/= `(,p . ,q) `("hello" . "world"))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-str _.0)))))

(test 'not-stringo-12d
  (run* (x)
    (fresh (p q)
      (not-stringo p)
      (=/= `("hello" . "world") `(,p . ,q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-str _.0)))))

(test 'not-stringo-12e
  (run* (x)
    (fresh (p q)
      (not-stringo p)
      (=/= `("hello" . ,q) `(,p . "world"))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-str _.0)))))

(test 'not-stringo-12f
  (run* (x)
    (fresh (p q)
      (not-stringo p)
      (=/= `(727 . ,q) `(,p . "wysi"))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 727) (_.1 "wysi"))) (not-str _.0)))))

(test 'not-stringo-13a
  (run* (x) (not-stringo x) (conde ((stringo x)) ((== x 'x))))
  '((x)))

(test 'not-stringo-13b
  (run* (x) (not-stringo x) (conde ((== x 13)) ((== x 'x))))
  '((13) (x)))
