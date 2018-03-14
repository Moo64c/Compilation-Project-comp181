;; Created by Amir Arbel and Ronen Finish

(define bound-var?

    (letrec (
			(v-in-expression
				(lambda (var_name expression)
				  (cond
						((null? expression)
							#f
						)
						((list? expression)
							(or
								(v-in-expression var_name (car expression))
								(v-in-expression var_name (cdr expression))
							)
						)
						((equal? var_name expression)
							#t
						)
						(else
							#f
						)
					)
			))
	    (rex
				(lambda (var_name expression)
				  (cond
						((null? expression)
							#f
						)
						((is-lambda-without-param? var_name expression)
							(v-in-expression var_name expression)
						)
						((list? expression)
							(or
								(rex var_name (car expression))
								(rex var_name (cdr expression))
							)
						)
						(else #f)
					)
				)
			)
		)
    (lambda (var_name scope)
			(rex var_name scope))
		)
)


(define is-set-expr?
	(lambda (var_name expression)
		(and
			(list? expression)
			(equal? 3 (length expression))
			(equal? 'set (car expression))
			(equal? var_name (cadr (cadr expression)))
		)
	)
)

(define exp-has-set?
  (lambda (var_name expression)
	  (cond
			((null? expression)
				#f
			)
		  ((is-lambda-with-param? var_name expression)
				#f
			)
		  ((is-set-expr? var_name expression)
				#t
			)
		  ((list? expression)
				(or
					(exp-has-set? var_name (car expression))
					(exp-has-set? var_name (cdr expression))
				)
			)
		  (else
				#f
			)
		)
	)
)

(define exp-has-get?
	(lambda (var_name expression)
		(cond
			((null? expression)
				#f
			)
			((is-lambda-with-param? var_name expression)
				#f
			)
			((is-set-expr? var_name expression)
				(exp-has-get? var_name (caddr expression))
			)
			((and
					(list? expression)
					(equal? 2 (length expression))
					(equal? 'var (car expression))
					(equal? var_name (cadr expression))
				)
				#t
			)
			((list? expression)
				(or
					(exp-has-get? var_name (car expression))
					(exp-has-get? var_name (cdr expression))
				)
			)
			(else
				#f
			)
		)
	)
)

(define box-param?
  (lambda (var_name scope)
	  (and
			(bound-var? var_name scope)
			(exp-has-set? var_name scope)
			(exp-has-get? var_name scope)
		)
	)
)

(define list-of-params-to-box
  (letrec
		(
			(list-of-params-to-box-recursive
				(lambda (expression _list)
					(cond
						((null? _list)
							'()
						)
			      ((box-param? (car _list) expression)
							(cons
								(car _list)
								(list-of-params-to-box-recursive expression (cdr _list))
							)
						)
			      (else (list-of-params-to-box-recursive expression (cdr _list))))
					)
			)
		)
    (lambda (scope list-of-vars)
	  	(reverse (list-of-params-to-box-recursive scope list-of-vars))
		)
	)
)

(define box-param-parser
  (letrec
		(
    	(box-param-parser-recursive
				(lambda (var_name expression)
					(cond
						((null? expression)
							'()
						)
		      	((is-lambda-with-param? var_name expression)
						 expression
					  )
			      ((is-set-expr? var_name expression)
							`(box-set ,(cadr expression) ,(box-param-parser-recursive var_name (caddr expression)))
						)
			      ((and
								(list? expression)
								(equal? 2 (length expression))
							  (equal? 'var (car expression))
								(equal? var_name (cadr expression))
							)
					    `(box-get ,expression)
						)
			      ((list? expression)
							(cons
								(box-param-parser-recursive var_name (car expression))
								(box-param-parser-recursive var_name (cdr expression))
							)
						)
			      (else expression)
					)
				)
			)
		)

		(lambda (var_name scope)
			(if (equal? 'seq (car scope))
				`(seq (
					  (set (var ,var_name) (box (var ,var_name)))
					  ,@(box-param-parser-recursive var_name (cadr scope))
				 	)
				)
		  ;else
				`(seq (
				   (set (var ,var_name) (box (var ,var_name)))
				   ,(box-param-parser-recursive var_name  scope)
				  )
			  )
			)
	  )
  )
)
