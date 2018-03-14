
(define loop (lambda (num func param)
                 (if (zero? num)
                     param
                     (loop (- num 1) func (func param))
                     )
                 )
    )
(loop 7 (lambda (x) (+ x x)) 43)