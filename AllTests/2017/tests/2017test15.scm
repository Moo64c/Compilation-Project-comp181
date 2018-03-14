(define rec (lambda (func1 func2 init param num)
                (if (= 0 num)
                    init
                    (func1 (rec func1 func2 (func2 2 init param) param (- num 1))
                      )
                    )
                )
    )
(rec - + 5 7 20)
