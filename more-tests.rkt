(display "\nRunning more tests")
(newline)

(test 'mixed-0
  (run* (x) (fresh (y) (=/= x y) (== x y) (symbolo x) (symbolo y)))
  '())

(test 'mixed-1
  (run* (x) (fresh (y) (symbolo x) (symbolo y) (=/= x y) (== x y)))
  '())
