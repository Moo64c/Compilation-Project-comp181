
(define f (lambda (x) (if (zero? x) x (+ 1 (f (- x 1))))))
(eq? 50 (f 50))