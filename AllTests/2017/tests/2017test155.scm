
(letrec ((loop (lambda (i a)
		 (set! a (+ (* 10 a) i))
		 (if (< i 10)
		     (loop (+ i 1) a)
		     a))))
  (loop 0 0))