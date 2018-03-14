(define begin?
  (lambda (sxpr)
    (and
      (list? sxpr)
      (< 0 (length sxpr))
      (equal? 'begin (car sxpr))
    )
  )
)

(define parse-begin
      (letrec
        ((rex
          (lambda (_listExp)
      		  (cond
              ((null? _listExp) (list))
      			  ((begin? (car _listExp)) (append (rex (cdr (car _listExp))) (rex (cdr _listExp))))
      			  (else (cons (parse (car _listExp) ) (rex (cdr _listExp))))
            )
          )
        )
       )
       (lambda (sxpr)
	      (cond
          ((= 1 (length sxpr)) `(const ,@(list (void))))
	        ((= 2 (length sxpr)) (parse (cadr sxpr)))
          (else `(seq ,(rex (cdr sxpr))))
        )
      )
    )
  )
