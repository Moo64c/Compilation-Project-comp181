
(define (accumulate op init lst)
    (if (null? lst)
        init
        (op (car lst) (accumulate op init (cdr lst)))))
(accumulate * 2 '(1 2 3 4 5 6 7 8 9))