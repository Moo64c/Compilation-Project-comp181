
(define (plusminus . l)
    (if (null? l) 0
        (if (null? (cdr l)) (car l)
        (+ (- (car l) (car (cdr l))) (apply plusminus (cdr (cdr l)))))))
(plusminus 5 4 8 6 7 2 3 0 5 4 8 9 0)