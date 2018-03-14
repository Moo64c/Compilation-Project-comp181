;; Created by Amir Arbel and Ronen Finish
(define is-lambda-nil?
  
  (lambda (expr)
    (and
      ; Applic statement
      (list? expr)
      (equal? 3 (length expr))
      (equal? 'applic (car expr))
      ; Lambda-nil statement
      (let ((lambda-expr (cadr expr)))
        (and
          (list? lambda-expr)
          (equal? 3 (length lambda-expr))
          (equal? (car lambda-expr) 'lambda-simple)
          ;params
          (list? (cadr lambda-expr))
          (equal? 0 (length (cadr lambda-expr)))
        )
      )
      ; Variable assignment list
      (list? (caddr expr))
      (equal? 0 (length (caddr expr)))
    )
  )
)

(define remove-applic-lambda-nil-parser

  (lambda (expr)
    ; Return a recursivly-parsed expression in the lambda body.
    (remove-applic-lambda-nil-wrapper (caddr (cadr expr)))
  )
)

(define remove-applic-lambda-nil-wrapper

  (lambda (expr)
	  (cond
      ((null? expr) '())
		  ((is-lambda-nil? expr) (remove-applic-lambda-nil-parser expr))
		  ((list? expr) (cons
        (remove-applic-lambda-nil-wrapper (car expr))
        (remove-applic-lambda-nil-wrapper (cdr expr)))
      )
		  (else expr)
    )
  )
)
