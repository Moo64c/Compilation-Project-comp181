
(define (less-than  . l)
     (cond
       ((null? l) #t)
       ((null? (cdr l)) #t)
       ((< (car l) (car (cdr l))) (apply less-than  (cdr l)))
       (else #f)))
	   
(less-than 5 4 8 9 6 2 5 4 4 44)