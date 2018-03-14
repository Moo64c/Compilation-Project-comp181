 (define fun2 (lambda (x)
                 (begin
                   (set! x (+ 2 1))
                   (set! x (+ x 3 4))
                   x
                   )
                 )
    )
(fun2 45)