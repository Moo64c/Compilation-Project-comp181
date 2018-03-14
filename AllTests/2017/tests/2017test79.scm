
(define foo10 (lambda (x y) (
                            begin
                            (set! y x)
                            (eq? y x))
                 )
    )
(foo10 12 12)