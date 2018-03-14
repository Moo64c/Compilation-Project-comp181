;@file let-reg.scm
; Hold let processing

(define _let-star?
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
        (equal? (car expr) 'let*)
        (list? (cadr expr))
        ; Check parameter validity.
        (params? (cadr expr))
      )
    )
  )
)
(define parse-let-star
  (letrec
    (
      (let*->let
        (lambda (args body)
          (if (= 1 (length args))
            `(let
            ,args
            ,@body)
            `(let
            (,(car args))
            ,(let*->let (cdr args) body)
            )
          )
        )
      )
    )
    (lambda (expr)
      (if (= 0 (length (cadr expr)))
       (parse `(let () ,@(cddr expr)))
       (parse (let*->let (cadr expr) (cddr expr)))
      )
    )
  )
)
