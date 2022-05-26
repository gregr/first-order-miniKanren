(display "\nRunning !stringo tests")
(newline)

(test '!stringo-0
  (run* (x) (!stringo x))
  '(#s(Ans (_.0) ((!str _.0)))))

(test '!stringo-1
  (run* (x) (!stringo "f"))
  '())

(test '!stringo-2-a
  (run* (x) (!stringo x) (== x 5))
  '((5)))

(test '!stringo-2-b
  (run* (x) (== x 5) (!stringo x))
  '((5)))

(test '!stringo-3-a
  (run* (x) (!stringo x) (== x "foo"))
  '())

(test '!stringo-3-b
  (run* (x) (== x "foo") (!stringo x))
  '())

(test '!stringo-4-a
  (run* (x) (fresh (q) (!stringo x) (== q x) (== q "bar")))
  '())

(test '!stringo-4-b
  (run* (x) (fresh (q) (!stringo x) (== q "bar") (== q x)))
  '())

(test '!stringo-4-c
  (run* (x) (fresh (q) (== q x) (!stringo x) (== q "bar")))
  '())

(test '!stringo-5-a
  (run* (x) (fresh (q) (!stringo x) (== q x) (== q 'a)))
  '((a)))

(test '!stringo-5-b
  (run* (x) (fresh (q) (!stringo x) (== q 'a) (== q x)))
  '((a)))

(test '!stringo-5-c
  (run* (x) (fresh (q) (== q x) (!stringo x) (== q 'a)))
  '((a)))

(test '!stringo-6-a
  (run* (x) (fresh (q) (!stringo x) (== q x) (!stringo q)))
  '(#s(Ans (_.0) ((!str _.0)))))

(test '!stringo-6-b
  (run* (x) (fresh (q) (!stringo x) (!stringo q) (== q x)))
  '(#s(Ans (_.0) ((!str _.0)))))

(test '!stringo-6-c
  (run* (x) (fresh (q) (== q x) (!stringo x) (!stringo q)))
  '(#s(Ans (_.0) ((!str _.0)))))

(test '!stringo-7-a
  (run* (x) (!stringo x) (=/= x "hello"))
  '(#s(Ans (_.0) ((!str _.0)))))

(test '!stringo-7-b
  (run* (x) (=/= x "hello") (!stringo x))
  '(#s(Ans (_.0) ((!str _.0)))))

(test '!stringo-8-a
  (run* (x) (!stringo x) (=/= x 5))
  '(#s(Ans (_.0) ((!str _.0) (=/= ((_.0 5)))))))

(test '!stringo-8-b
  (run* (x) (=/= x 5) (!stringo x))
  '(#s(Ans (_.0) ((!str _.0) (=/= ((_.0 5)))))))

(test '!stringo-9a
  (run* (x)
    (fresh (p q)
      (!stringo p)
      (!stringo q)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!str _.0) (!str _.1)))))

(test '!stringo-9b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (!stringo p)
      (!stringo q)))
  '(#s(Ans ((_.0 _.1)) ((!str _.0) (!str _.1)))))

(test '!stringo-10a
  (run* (x)
    (fresh (p q)
      (!stringo p)
      (!stringo p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!str _.0)))))

(test '!stringo-10b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (!stringo p)
      (!stringo p)))
  '(#s(Ans ((_.0 _.1)) ((!str _.0)))))

(test '!stringo-10c
  (run* (x)
    (fresh (p q)
      (!stringo p)
      (== `(,p ,q) x)
      (!stringo p)))
  '(#s(Ans ((_.0 _.1)) ((!str _.0)))))

(test '!stringo-11a
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!stringo p)
      (!stringo q)))
  '((_.0)))

(test '!stringo-11b
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!stringo p)
      (!stringo q)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((!str _.0) (!str _.1) (=/= ((_.0 _.2) (_.1 _.3)))))))

(test '!stringo-11c
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!stringo p)
      (!stringo r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((!str _.0) (!str _.2) (=/= ((_.0 _.2) (_.1 _.3)))))))

(test '!stringo-11d
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!stringo p)
      (!stringo r)
      (== p r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.0 _.2)) ((!str _.0) (=/= ((_.1 _.2)))))))

(test '!stringo-12a
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `("hello" . "world"))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 "hello") (_.1 "world")))))))

(test '!stringo-12b
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `("hello" . "world"))
      (!stringo p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!str _.0)))))

(test '!stringo-12c
  (run* (x)
    (fresh (p q)
      (!stringo p)
      (=/= `(,p . ,q) `("hello" . "world"))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!str _.0)))))

(test '!stringo-12d
  (run* (x)
    (fresh (p q)
      (!stringo p)
      (=/= `("hello" . "world") `(,p . ,q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!str _.0)))))

(test '!stringo-12e
  (run* (x)
    (fresh (p q)
      (!stringo p)
      (=/= `("hello" . ,q) `(,p . "world"))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!str _.0)))))

(test '!stringo-12f
  (run* (x)
    (fresh (p q)
      (!stringo p)
      (=/= `(727 . ,q) `(,p . "wysi"))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!str _.0) (=/= ((_.0 727) (_.1 "wysi")))))))