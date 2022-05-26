(display "\nRunning !symbolo tests")
(newline)

(test '!symbolo-0
  (run* (x) (!symbolo x))
  '(#s(Ans (_.0) ((!sym _.0)))))

(test '!symbolo-1
  (run* (x) (!symbolo 'x))
  '())

(test '!symbolo-2-a
  (run* (x) (!symbolo x) (== x "hello"))
  '(("hello")))

(test '!symbolo-2-b
  (run* (x) (== x "hello") (!symbolo x))
  '(("hello")))

(test '!symbolo-3-a
  (run* (x) (!symbolo x) (== x 'x))
  '())

(test '!symbolo-3-b
  (run* (x) (== x 'x) (!symbolo x))
  '())

(test '!symbolo-4-a
  (run* (x) (fresh (q) (!symbolo x) (== q x) (== q 'q)))
  '())

(test '!symbolo-4-b
  (run* (x) (fresh (q) (!symbolo x) (== q 'q) (== q x)))
  '())

(test '!symbolo-4-c
  (run* (x) (fresh (q) (== q x) (!symbolo x) (== q 'q)))
  '())

(test '!symbolo-5-a
  (run* (x) (fresh (q) (!symbolo x) (== q x) (== q "a")))
  '(("a")))

(test '!symbolo-5-b
  (run* (x) (fresh (q) (!symbolo x) (== q "a") (== q x)))
  '(("a")))

(test '!symbolo-5-c
  (run* (x) (fresh (q) (== q x) (!symbolo x) (== q "a")))
  '(("a")))

(test '!symbolo-6-a
  (run* (x) (fresh (q) (!symbolo x) (== q x) (!symbolo q)))
  '(#s(Ans (_.0) ((!sym _.0)))))

(test '!symbolo-6-b
  (run* (x) (fresh (q) (!symbolo x) (!symbolo q) (== q x)))
  '(#s(Ans (_.0) ((!sym _.0)))))

(test '!symbolo-6-c
  (run* (x) (fresh (q) (== q x) (!symbolo x) (!symbolo q)))
  '(#s(Ans (_.0) ((!sym _.0)))))

(test '!symbolo-7-a
  (run* (x) (!symbolo x) (=/= x 'x))
  '(#s(Ans (_.0) ((!sym _.0)))))

(test '!symbolo-7-b
  (run* (x) (=/= x 'x) (!symbolo x))
  '(#s(Ans (_.0) ((!sym _.0)))))

(test '!symbolo-8-a
  (run* (x) (!symbolo x) (=/= x 8))
  '(#s(Ans (_.0) ((!sym _.0) (=/= ((_.0 8)))))))

(test '!symbolo-8-b
  (run* (x) (=/= x 8) (!symbolo x))
  '(#s(Ans (_.0) ((!sym _.0) (=/= ((_.0 8)))))))

(test '!symbolo-9a
  (run* (x)
    (fresh (p q)
      (!symbolo p)
      (!symbolo q)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.0) (!sym _.1)))))

(test '!symbolo-9b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (!symbolo p)
      (!symbolo q)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.0) (!sym _.1)))))

(test '!symbolo-10a
  (run* (x)
    (fresh (p q)
      (!symbolo p)
      (!symbolo p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.0)))))

(test '!symbolo-10b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (!symbolo p)
      (!symbolo p)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.0)))))

(test '!symbolo-10c
  (run* (x)
    (fresh (p q)
      (!symbolo p)
      (== `(,p ,q) x)
      (!symbolo p)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.0)))))

(test '!symbolo-11a
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!symbolo p)
      (!symbolo q)))
  '((_.0)))

(test '!symbolo-11b
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!symbolo p)
      (!symbolo q)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((!sym _.0) (!sym _.1) (=/= ((_.0 _.2) (_.1 _.3)))))))

(test '!symbolo-11c
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!symbolo p)
      (!symbolo r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((!sym _.0) (!sym _.2) (=/= ((_.0 _.2) (_.1 _.3)))))))

(test '!symbolo-11d
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!symbolo p)
      (!symbolo r)
      (== p r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.0 _.2)) ((!sym _.0) (=/= ((_.1 _.2)))))))

(test '!symbolo-12a
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `(p . q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 p) (_.1 q)))))))

(test '!symbolo-12b
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `(p . q))
      (!symbolo p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.0)))))

(test '!symbolo-12c
  (run* (x)
    (fresh (p q)
      (!symbolo p)
      (=/= `(,p . ,q) `(p . q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.0)))))

(test '!symbolo-12d
  (run* (x)
    (fresh (p q)
      (!symbolo p)
      (=/= `(p . q) `(,p . ,q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.0)))))

(test '!symbolo-12e
  (run* (x)
    (fresh (p q)
      (!symbolo p)
      (=/= `(p . ,q) `(,p . q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.0)))))

(test '!symbolo-12f
  (run* (x)
    (fresh (p q)
      (!symbolo p)
      (=/= `("q" . ,q) `(,p . 'p))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.0) (=/= ((_.0 "q") (_.1 'p)))))))