
(define (func . numbers)
    (if (null? numbers)
        0
        (+ (car numbers) (apply func (cdr numbers)))))
(func 9 8 7 6 5 4)