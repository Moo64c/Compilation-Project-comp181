 (define (=number? exp num)
  (and (number? exp) (= exp num)))
(=number? 5 1)
