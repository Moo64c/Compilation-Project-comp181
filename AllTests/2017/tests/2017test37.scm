(define (a x set)
  (cond
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (cons (car set)(a x (cdr set))))))
	
(a 3 (cons 5 4))
