
(define foo7 (lambda (x y) (
                            begin
                            (set! y x)
                            (set! x y)
                            (+ y x))
                 )
    )
(foo7 1 3)