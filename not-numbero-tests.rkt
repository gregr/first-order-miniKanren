(display "\nRunning !numbero tests")
(newline)

(test '!numbero-0
  (run* (x) (!numbero x))
  '(#s(Ans (_.0) ((!num _.0)))))

(test '!numbero-1
  (run* (x) (!numbero 1))
  '())

(test '!numbero-2-a
  (run* (x) (!numbero x) (== x "hello"))
  '(("hello")))

(test '!numbero-2-b
  (run* (x) (== x "hello") (!numbero x))
  '(("hello")))

(test '!numbero-3-a
  (run* (x) (!numbero x) (== x 3))
  '())

(test '!numbero-3-b
  (run* (x) (== x 3) (!numbero x))
  '())

(test '!numbero-4-a
  (run* (x) (fresh (q) (!numbero x) (== q x) (== q 4)))
  '())

(test '!numbero-4-b
  (run* (x) (fresh (q) (!numbero x) (== q 4) (== q x)))
  '())

(test '!numbero-4-c
  (run* (x) (fresh (q) (== q x) (!numbero x) (== q 4)))
  '())

(test '!numbero-5-a
  (run* (x) (fresh (q) (!numbero x) (== q x) (== q 'a)))
  '((a)))

(test '!numbero-5-b
  (run* (x) (fresh (q) (!numbero x) (== q 'a) (== q x)))
  '((a)))

(test '!numbero-5-c
  (run* (x) (fresh (q) (== q x) (!numbero x) (== q 'a)))
  '((a)))

(test '!numbero-6-a
  (run* (x) (fresh (q) (!numbero x) (== q x) (!numbero q)))
  '(#s(Ans (_.0) ((!num _.0)))))

(test '!numbero-6-b
  (run* (x) (fresh (q) (!numbero x) (!numbero q) (== q x)))
  '(#s(Ans (_.0) ((!num _.0)))))

(test '!numbero-6-c
  (run* (x) (fresh (q) (== q x) (!numbero x) (!numbero q)))
  '(#s(Ans (_.0) ((!num _.0)))))

(test '!numbero-7-a
  (run* (x) (!numbero x) (=/= x 7))
  '(#s(Ans (_.0) ((!num _.0)))))

(test '!numbero-7-b
  (run* (x) (=/= x 7) (!numbero x))
  '(#s(Ans (_.0) ((!num _.0)))))

(test '!numbero-8-a
  (run* (x) (!numbero x) (=/= x 'a))
  '(#s(Ans (_.0) ((!num _.0) (=/= ((_.0 a)))))))

(test '!numbero-8-b
  (run* (x) (=/= x 'a) (!numbero x))
  '(#s(Ans (_.0) ((!num _.0) (=/= ((_.0 a)))))))

(test '!numbero-9a
  (run* (x)
    (fresh (p q)
      (!numbero p)
      (!numbero q)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0) (!num _.1)))))

(test '!numbero-9b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (!numbero p)
      (!numbero q)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0) (!num _.1)))))

(test '!numbero-10a
  (run* (x)
    (fresh (p q)
      (!numbero p)
      (!numbero p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0)))))

(test '!numbero-10b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (!numbero p)
      (!numbero p)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0)))))

(test '!numbero-10c
  (run* (x)
    (fresh (p q)
      (!numbero p)
      (== `(,p ,q) x)
      (!numbero p)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0)))))

(test '!numbero-11a
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!numbero p)
      (!numbero q)))
  '((_.0)))

(test '!numbero-11b
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!numbero p)
      (!numbero q)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((!num _.0) (!num _.1) (=/= ((_.0 _.2) (_.1 _.3)))))))

(test '!numbero-11c
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!numbero p)
      (!numbero r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.2 _.3)) ((!num _.0) (!num _.2) (=/= ((_.0 _.2) (_.1 _.3)))))))

(test '!numbero-11d
  (run* (x)
    (fresh (p q r s)
      (=/= `(,p . ,q) `(,r . ,s))
      (!numbero p)
      (!numbero r)
      (== p r)
      (== `(,p ,q ,r ,s) x)))
  '(#s(Ans ((_.0 _.1 _.0 _.2)) ((!num _.0) (=/= ((_.1 _.2)))))))

(test '!numbero-12a
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `(12 . 13))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 12) (_.1 13)))))))

(test '!numbero-12b
  (run* (x)
    (fresh (p q)
      (=/= `(,p . ,q) `(12 . 13))
      (!numbero p)
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0)))))

(test '!numbero-12c
  (run* (x)
    (fresh (p q)
      (!numbero p)
      (=/= `(,p . ,q) `(12 . 13))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0)))))

(test '!numbero-12d
  (run* (x)
    (fresh (p q)
      (!numbero p)
      (=/= `(12 . 13) `(,p . ,q))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0)))))

(test '!numbero-12e
  (run* (x)
    (fresh (p q)
      (!numbero p)
      (=/= `(12 . ,q) `(,p . 13))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0)))))

(test '!numbero-12f
  (run* (x)
    (fresh (p q)
      (!numbero p)
      (=/= `("wysi" . ,q) `(,p . 727))
      (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0) (=/= ((_.0 "wysi") (_.1 727)))))))