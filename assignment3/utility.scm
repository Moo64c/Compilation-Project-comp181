;; Created by Amir Arbel and Ronen Finish

(define get-lambda-params (lambda (lmd)

  (cond
    ((equal? 'lambda-simple (car lmd)) (cadr lmd))
    ((equal? 'lambda-var (car lmd)) (list (cadr lmd)))
    ((equal? 'lambda-opt (car lmd))  (append (cadr lmd) (list (caddr lmd))))
    (else '())
  ))
)
(define get-lambda-body (lambda (lmd)
  (cond
    ((or
      (equal? 'lambda-simple (car lmd))
      (equal? 'lambda-var (car lmd))
     )
     (caddr lmd))
    (else (cadddr lmd))
  ))
)

; Is variable in an expression.
(define var-in-expr?

  (lambda (var_name expr)
    (cond
      ; Not found.
      ((null? expr) #f)
      ; Found!
      ; Recursive-call for the function.
      ((list? expr)
        (or
          (var-in-expr? var_name (car expr))
          (var-in-expr? var_name (cdr expr))
        )
      )
      ((equal? expr var_name) #t)
      ; Some other case.
      (else #f)
    )
  )
)

; Checks variable is a parameter in the expression's lambda.
(define var-is-parameter?
  (lambda (var_name expr)
    (member var_name (get-lambda-params expr))
  )
)

; Checks variable is not a parameter in the expression's lambda.
(define var-is-not-parameter?
  (lambda (var_name expr)
    (not (var-is-parameter? var_name expr))
  )
)


; Helper function.
(define _is-lambda-?

  (lambda (var_name expr checker)
    (and
      (list? expr)
      (< 2 (length expr))
      (checker var_name expr)
      (or
          (equal? 'lambda-simple (car expr))
          (equal? 'lambda-var (car expr))
          (equal? 'lambda-opt (car expr))
      )
    )
  )
)

; Check if the expression is a lambda.
(define lambda?
  (lambda (expr)
    (_is-lambda-? 'null expr (lambda (var_name expr) #t))
  )
)
(define header
  (lambda (expr)
    (if (equal? 'lambda-opt (car expr))
      (list (car expr) (cadr expr) (caddr expr))
      (list (car expr) (cadr expr))
    )
  )
)
; Check if the expression is a lambda with a parameter.
(define is-lambda-with-param?

  (lambda (var_name expr)
    (_is-lambda-? var_name expr var-is-parameter?)
  )
)

; Check if the expression is a lambda without a parameter.
(define is-lambda-without-param?

  (lambda (var_name expr)
    (_is-lambda-? var_name expr var-is-not-parameter?)
  )
)
; Check if the expression is a lambda without a params.
(define is-lambda-without-params?

  (lambda (expr)
    (_is-lambda-? 'null expr (lambda (var_name expr) (< 0 (length (get-lambda-params expr)))))
  )
)
