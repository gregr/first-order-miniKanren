(display "\nRunning !symbolo/!numbero tests")
(newline)

(test '!symbolo-!numbero-0
  (run* (x y) (!symbolo x) (!numbero y))
  '(#s(Ans (_.0 _.1) ((!num _.1) (!sym _.0)))))

(test '!symbolo-!numbero-1
  (run* (x) (!symbolo x) (!numbero x))
  '(#s(Ans (_.0) ((!num _.0) (!sym _.0)))))

(test '!symbolo-!numbero-2a
  (run* (x y) (!symbolo x) (!numbero y) (== x y))
  '(#s(Ans (_.0 _.0) ((!num _.0) (!sym _.0)))))

(test '!symbolo-!numbero-2b
  (run* (x y) (!symbolo x) (== x y) (!numbero y))
  '(#s(Ans (_.0 _.0) ((!num _.0) (!sym _.0)))))

(test '!symbolo-!numbero-2c
  (run* (x y) (== x y) (!symbolo x) (!numbero y))
  '(#s(Ans (_.0 _.0) ((!num _.0) (!sym _.0)))))

(test '!symbolo-!numbero-3
  (run* (x y) (== x y) (!symbolo x) (!numbero y))
  '(#s(Ans (_.0 _.0) ((!num _.0) (!sym _.0)))))

(test '!symbolo-!numbero-4a
  (run* (x) (fresh (p) (!numbero p) (== p 'p)))
  '((_.0)))

(test '!symbolo-!numbero-4a
  (run* (x) (fresh (p) (!symbolo p) (== p 'p)))
  '())

(test '!symbolo-!numbero-5
  (run* (x) (!numbero x) (conde ((== x 3)) ((== x 'x))))
  '((x)))

(test '!symbolo-!numbero-6a
  (run* (x) (fresh (p) (!symbolo x) (!numbero p) (== p x)))
  '(#s(Ans (_.0) ((!num _.0) (!sym _.0)))))

(test '!symbolo-!numbero-6b
  (run* (x) (fresh (p) (!symbolo x) (== p x) (!numbero p)))
  '(#s(Ans (_.0) ((!num _.0) (!sym _.0)))))

(test '!symbolo-!numbero-6c
  (run* (x) (fresh (p) (== p x) (!symbolo x) (!numbero p)))
  '(#s(Ans (_.0) ((!num _.0) (!sym _.0)))))

(test '!symbolo-!numbero-7a
  (run* (x) (fresh (p q)
              (!symbolo p)
              (!numbero q)
              (== `(,p ,q) x)))
  '(#s(Ans ((_.0 _.1)) ((!num _.1) (!sym _.0)))))

(test '!symbolo-!numbero-7b
  (run* (x) (fresh (p q)
              (!symbolo p)
              (== `(,p ,q) x)
              (!numbero q)))
  '(#s(Ans ((_.0 _.1)) ((!num _.1) (!sym _.0)))))

(test '!symbolo-!numbero-7c
  (run* (x) (fresh (p q)
              (== `(,p ,q) x)
              (!symbolo p)
              (!numbero q)))
  '(#s(Ans ((_.0 _.1)) ((!num _.1) (!sym _.0)))))

(test '!symbolo-!numbero-7d
  (run* (x) (fresh (p q)
              (!symbolo p)
              (!numbero q)
              (== `(,p ,q) x))
            (fresh (r s)
              (== `(,r ,s) x)))
  '(#s(Ans ((_.0 _.1)) ((!num _.1) (!sym _.0)))))

(test '!symbolo-!numbero-7d
  (run* (x) (fresh (p q)
              (!symbolo p)
              (!numbero q)
              (== `(,p ,q) x))
            (fresh (r s)
              (== `(,r ,s) x)
              (== r 5)))
  '(#s(Ans ((5 _.0)) ((!num _.0)))))

(test '!symbolo-!numbero-7e
  (run* (x) (fresh (p q)
              (!symbolo p)
              (!numbero q)
              (== `(,p ,q) x))
            (fresh (r s)
              (== `(,r ,s) x)
              (== s 's)))
  '(#s(Ans ((_.0 s)) ((!sym _.0)))))

(test '!symbolo-!numbero-8a
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (=/= `(5 a) x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 5) (_.1 a)))))))

(test '!symbolo-!numbero-8b
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (=/= `(5 a) x)
      (!numbero p)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0)))))

(test '!symbolo-!numbero-8c
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (!numbero p)
      (=/= `(5 a) x)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0)))))

(test '!symbolo-!numbero-8d
  (run* (x)
    (fresh (p q)
      (!numbero p)
      (== `(,p ,q) x)
      (=/= `(5 a) x)))
  '(#s(Ans ((_.0 _.1)) ((!num _.0)))))

(test '!symbolo-!numbero-8e
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (=/= `(5 a) x)
      (!symbolo q)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.1)))))

(test '!symbolo-!numbero-8f
  (run* (x)
    (fresh (p q)
      (== `(,p ,q) x)
      (!symbolo q)
      (=/= `(5 a) x)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.1)))))

(test '!symbolo-!numbero-8g
  (run* (x)
    (fresh (p q)
      (!symbolo q)
      (== `(,p ,q) x)
      (=/= `(5 a) x)))
  '(#s(Ans ((_.0 _.1)) ((!sym _.1)))))