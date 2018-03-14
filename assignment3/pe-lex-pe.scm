;; Created by Amir Arbel and Ronen Finish
(define pe-lex-pe-wrapper
  (lambda (expr)
    (letrec
      ((anot (annotate-expression expr))
      (recursive-pe-wrappper (lambda (expr)
        (cond
          ((null? expr) '())
          ((and
            (list? expr)
            (= 2 (length expr))
            (equal? 'var (car expr)))
            (cons
              'fvar
              (cdr expr)
            )
          )
        ((list? expr)
          (cons
            (recursive-pe-wrappper (car expr))
            (recursive-pe-wrappper (cdr expr))
          )
        )
        (else expr)
      ))
    ))
    (recursive-pe-wrappper anot)
    )
  )
)

(define annotate-expression
	(lambda (expr)
	  (cond
      ((null? expr)
        '()
      )
		  ((is-lambda-without-params? expr)
        (wrap-expression expr)
      )
	    ((list? expr)
        (cons
          (annotate-expression (car expr))
          (annotate-expression (cdr expr))
        )
      )
	    (else expr)
    )
  )
)

(define annotate-scope
  (lambda (scope reversed-_list index)
    (rename-vars scope reversed-_list `(bvar ,index) reversed-_list index)
  )
)

(define wrap-expression
  (lambda (expr)
    (append
      (header expr)
      (list
        (rename-vars
          (get-lambda-body expr)
          (reverse (get-lambda-params expr))
          `(pvar)
          (reverse (get-lambda-params expr))
          -1
        )
      )
    )
  )
)


(define rename-vars
  (let
    (
      (couple-first-last-in-list
        (lambda (_list)
          (cons
            (car _list)
            (cons
              (car (reverse (cdr _list)))
              (reverse (cdr (reverse (cdr _list))) )
            )
          )
        )
      )
    )
    (lambda (scope reversed-_list prefix _list index)
	    (cond
        ((null? reversed-_list) scope)
	      (else
          (wrap-var-by-name
            (car reversed-_list)
            (couple-first-last-in-list
              (append
                prefix
                (list
                  (- (length reversed-_list) 1)
                  (car reversed-_list)
                )
              )
            )
			      (rename-vars scope (cdr reversed-_list) prefix _list index)
            _list
            index
          )
        )
      )
    )
  )
)

(define wrap-var-by-name
  (lambda (var_name address expr _list index)
    (cond
      ((null? expr)
        '()
      )
      ((lambda? expr)
        (append
          (header (wrap-expression expr))
          (list
            (annotate-scope
              (get-lambda-body (wrap-expression expr))
              _list
              (+ index 1)
            )
          )
        )
      )
      (
        (and
          (list? expr)
          (equal? 'var (car expr))
          (equal? var_name (cadr expr))
        )
        address
      )
      ((list? expr)
        (cons
          (wrap-var-by-name var_name address (car expr) _list index)
          (wrap-var-by-name var_name address (cdr expr) _list index)
        )
      )
      (else expr)
    )
  )
)

(define bound-var?
    (letrec
      (
	    (rex (lambda (var_name expr)
  		  (cond
          ((null? expr) #f)
  			  ((var-is-not-parameter? var_name expr) (var-in-expr? var_name expr))
  			  ((list? expr)
            (or
              (rex var_name (car expr))
              (rex var_name (cdr expr))
            )
          )
  			  (else #f)
        ))
      )
    )
    (lambda (var_name scope)
	   (rex var_name scope)
    )
  )
)
