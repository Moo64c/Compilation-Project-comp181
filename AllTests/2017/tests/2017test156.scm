
(define func (lambda (lst num) (
                                  letrec ((loop
                                             (lambda (i a)
                                               (cond ((null? i)
                                                      #f)
                                                 ((eq? (car i) a) #t)
                                                 (else
                                                   (loop (cdr i) a)))
                                               )))
                                    (loop lst num)))
                 )
(func (list 1 2 3) 5)