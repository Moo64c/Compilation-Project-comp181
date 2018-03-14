
(define a (lambda (x y) (if (not (zero? x)) (denominator (/ y x)) (numerator y))))
(a 0 5)