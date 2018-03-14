
(define (b set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
     (let ((s1 (car set1))
           (s2 (car set2)))
       (cond
       ((= s1 s2) (cons s1 (b (cdr set1) (cdr set2))))
       ((> s1 s2) (cons s2 (b set1 (cdr set2))))
       ((< s1 s2) (cons s1 (b (cdr set1) set2))))))))
(b '(1 2 3) '(4 5 6))