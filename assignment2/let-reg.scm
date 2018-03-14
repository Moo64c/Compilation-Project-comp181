;@file let-reg.scm
; Hold let processing

(define _let?
  (lambda (expr)
    (letrec (
        (params?
          (lambda (_list)
            (cond
              ((null? _list)
                #t)
              ((and
                (pair? (car _list))
                (_var? (caar _list)))
                (params? (cdr _list))
              )
              (else
                #f)
            )
          )
        )
        ; Pull variable names out of list.
        (proc-vars
          (lambda (_list)
            (if (null? _list)
              '()
              (cons (caar _list) (proc-vars (cdr _list)))
            )
          )
        )
      )
      (and
        (list? expr)
        (< 2 (length expr))
        (equal? (car expr) 'let)
        (list? (cadr expr))
        ; Check parameter validity.
        (params? (cadr expr))
        (check-no-double-vars (proc-vars (cadr expr)))
      )
    )
  )
)

(define parse-let
  (lambda (expr)
    (letrec (
      (params-list
        (lambda (_list)
          (cond
            ((null? _list)
              '())
            (else
              (cons (caar _list) (params-list (cdr _list)))
          )
        )
      ))
      (params-value
        (lambda (_list)
          (cond
            ((null? _list)
              '())
            (else
              (cons (cadar _list) (params-value (cdr _list)))
            )
          )
        )
      )
    )
    ; Use the parse function to macro-expand with lambda and applic.
    (parse `(
      (lambda
      ; List of var names
      ,(params-list (cadr expr))
      ; Expressions in the let body
      ,@(cddr expr))
      ; use applic to call the lambda
      ,@(params-value (cadr expr))))
    )
  )
)
