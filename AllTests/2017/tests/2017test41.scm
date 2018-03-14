
(let ((z 2))
  (define x (lambda (x) (lambda (y z) (y x))))
  (((x (lambda () z)) (lambda (z) z) 3))
)