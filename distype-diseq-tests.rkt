(display "\nRunning distype-diseq tests")
(newline)

(test 'distype-diseq-0
  (run* (x) (!numbero x) (=/= x 5))
  '(#s(Ans (_.0) ((!num _.0))))
)

(test 'distype-diseq-1
  (run* (x) (!symbolo x) (=/= x 5))
  '(#s(Ans (_.0) ((!sym _.0) (=/= ((_.0 5))))))
)

(test 'distype-diseq-2
  (run* (x) (!stringo x) (=/= x 5))
  '(#s(Ans (_.0) ((!str _.0) (=/= ((_.0 5))))))
)

(test 'distype-diseq-3
  (run* (x) (!numbero x) (=/= x 'a))
  '(#s(Ans (_.0) ((!num _.0) (=/= ((_.0 a))))))
)

(test 'distype-diseq-4
  (run* (x) (!symbolo x) (=/= x 'a))
  '(#s(Ans (_.0) ((!sym _.0))))
)

(test 'distype-diseq-5
  (run* (x) (!stringo x) (=/= x 'a))
  '(#s(Ans (_.0) ((!str _.0) (=/= ((_.0 a))))))
)

(test 'distype-diseq-6
  (run* (x) (!numbero x) (=/= x "test"))
  '(#s(Ans (_.0) ((!num _.0) (=/= ((_.0 "test"))))))
)

(test 'distype-diseq-7
  (run* (x) (!symbolo x) (=/= x "test"))
  '(#s(Ans (_.0) ((!sym _.0) (=/= ((_.0 "test"))))))
)

(test 'distype-diseq-8
  (run* (x) (!stringo x) (=/= x "test"))
  '(#s(Ans (_.0) ((!str _.0))))
)

(test 'distype-diseq-9
  (run* (x) (!stringo x) (!numbero x) (!symbolo x) (=/= x 5) (=/= x 'a) (=/= x "test"))
  '(#s(Ans (_.0) ((!num _.0) (!str _.0) (!sym _.0))))
)

(test 'distype-diseq-10
  (run* (x) (fresh (a) (!numbero x) (!stringo a) (== x a)))
  '(#s(Ans (_.0) ((!num _.0) (!str _.0))))
)