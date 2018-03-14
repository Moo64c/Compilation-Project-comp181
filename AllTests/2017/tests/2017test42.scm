
((lambda (z)
     (define x (lambda (xv) (lambda (y z) (y xv))))

     (((x (lambda () z)) (lambda (zv) zv) 3))
     ) 14)