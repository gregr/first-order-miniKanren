(display "\nRunning not-symbolo/not-numbero tests")
(newline)

(test 'not-symbolo-not-numbero-0
  (run* (x y) (not-symbolo x) (not-numbero y))
  '(#s(Ans (_.0 _.1) ((not-num _.1) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-1
  (run* (x) (not-symbolo x) (not-numbero x))
  '(#s(Ans (_.0) ((not-num _.0) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-2a
  (run* (x y) (not-symbolo x) (not-numbero y) (== x y))
  '(#s(Ans (_.0 _.0) ((not-num _.0) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-2b
  (run* (x y) (not-symbolo x) (== x y) (not-numbero y))
  '(#s(Ans (_.0 _.0) ((not-num _.0) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-2c
  (run* (x y) (== x y) (not-symbolo x) (not-numbero y))
  '(#s(Ans (_.0 _.0) ((not-num _.0) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-3
  (run* (x y) (== x y) (not-symbolo x) (not-numbero y))
  '(#s(Ans (_.0 _.0) ((not-num _.0) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-4a
  (run* (x) (fresh (p) (not-numbero p) (== p 'p)))
  '((_.0)))

(test 'not-symbolo-not-numbero-4a
  (run* (x) (fresh (p) (not-symbolo p) (== p 'p)))
  '())

(test 'not-symbolo-not-numbero-5a
  (run* (x) (not-numbero x) (conde ((== x 3)) ((== x 'x))))
  '((x)))

(test 'not-symbolo-not-numbero-5b
  (run* (x) (not-numbero x) (conde ((numbero x)) ((== x 'x))))
  '((x)))

(test 'not-symbolo-not-numbero-5c
  (run* (x) (not-numbero x) (conde ((numbero x)) ((not-symbolo x))))
  '(#s(Ans (_.0) ((not-num _.0) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-5d
  (run* (x) (not-numbero x) (conde ((not-symbolo x)) ((numbero x))))
  '(#s(Ans (_.0) ((not-num _.0) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-6a
  (run* (x) (fresh (p) (not-symbolo x) (not-numbero p) (== p x)))
  '(#s(Ans (_.0) ((not-num _.0) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-6b
  (run* (x) (fresh (p) (not-symbolo x) (== p x) (not-numbero p)))
  '(#s(Ans (_.0) ((not-num _.0) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-6c
  (run* (x) (fresh (p) (== p x) (not-symbolo x) (not-numbero p)))
  '(#s(Ans (_.0) ((not-num _.0) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-7a
  (run* (x) (fresh (p q)
              (not-symbolo p)
              (not-numbero q)
              (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.1) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-7b
  (run* (x) (fresh (p q)
              (not-symbolo p)
              (== `(,p ,q) x)
              (not-numbero q)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.1) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-7c
  (run* (x) (fresh (p q)
              (== `(,p ,q) x)
              (not-symbolo p)
              (not-numbero q)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.1) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-7d
  (run* (x) (fresh (p q)
              (not-symbolo p)
              (not-numbero q)
              (== `(,p ,q) x))
            (fresh (r s)
              (== `(,r ,s) x)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.1) (not-sym _.0)))))

(test 'not-symbolo-not-numbero-7d
  (run* (x) (fresh (p q)
              (not-symbolo p)
              (not-numbero q)
              (== `(,p ,q) x))
            (fresh (r s)
              (== `(,r ,s) x)
              (== r 5)))
  '(#s(Ans ((5 _.0)) ((not-num _.0)))))

(test 'not-symbolo-not-numbero-7e
  (run* (x) (fresh (p q)
              (not-symbolo p)
              (not-numbero q)
              (== `(,p ,q) x))
            (fresh (r s)
              (== `(,r ,s) x)
              (== s 's)))
  '(#s(Ans ((_.0 s)) ((not-sym _.0)))))

(test 'not-symbolo-not-numbero-8a
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (=/= `(5 a) x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 5) (_.1 a)))))))

(test 'not-symbolo-not-numbero-8b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (=/= `(5 a) x)
      (not-numbero p)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0)))))

(test 'not-symbolo-not-numbero-8c
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (not-numbero p)
      (=/= `(5 a) x)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0)))))

(test 'not-symbolo-not-numbero-8d
  (run* (x)
    (fresh (p q)
      (not-numbero p)
      (== `(,p ,q) x)
      (=/= `(5 a) x)))
  '(#s(Ans ((_.0 _.1)) ((not-num _.0)))))

(test 'not-symbolo-not-numbero-8e
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (=/= `(5 a) x)
      (not-symbolo q)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.1)))))

(test 'not-symbolo-not-numbero-8f
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (not-symbolo q)
      (=/= `(5 a) x)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.1)))))

(test 'not-symbolo-not-numbero-8g
  (run* (x)
    (fresh (p q)
      (not-symbolo q)
      (== `(,p ,q) x)
      (=/= `(5 a) x)))
  '(#s(Ans ((_.0 _.1)) ((not-sym _.1)))))