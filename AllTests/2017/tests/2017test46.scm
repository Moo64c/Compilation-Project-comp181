
(define loop2 (lambda (num func param)
                  (if (zero? num)
                      param
                      (func (loop2 (- num 1) func param)
                        )
                      )
                  )
    )
(loop2 7 (lambda (x) (+ x x)) 3)