;@file let-rec.scm
; Hold letrec processing

(define _letrec?
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
        (params-list
          (lambda (_list)
            (if (null? _list)
              '()
              (cons (caar _list) (params-list (cdr _list)))
            )
          )
        )
      )
      (and
        (list? expr)
        (< 2 (length expr))
        (equal? (car expr) 'letrec)
        (list? (cadr expr))
        ; Check parameter validity.
        (params? (cadr expr))
        (check-no-double-vars (params-list (cadr expr)))
      )
    )
  )
)

(define parse-letrec
    (letrec (
        (params-list
          (lambda (_list)
            (if (null? _list)
              '()
              (cons (caar _list) (params-list (cdr _list)))
            )
          )
        )
        (param-values
          (lambda (_list)
            (if (null? _list)
              '()
              (cons (cadar _list) (param-values (cdr _list)))
            )
          )
        )
    		(let-body-wrapper
          (lambda (expr)
  		      `(,(let-body expr) )
          )
        )
        (let-body
          (lambda (expr)
            `(lambda ,(list) ,@(cddr expr))
          )
        )
      )
      (lambda (expr)
        (parse
          `(let
            ; Outside wrapper.
            ,(map (lambda (v)
              (list v #f)
              )
              (params-list (cadr expr)))
            ; Internal part
            ,@(map (lambda (var val)
            ; Inside lambda with defined variables.
            ; Add (set [var] [val]) calls for applying data cleanly.
              `(set! ,var ,val)
              )
               (params-list (cadr expr))
               (param-values (cadr expr)))
            ,(let-body-wrapper expr)
          )
        )
    )
  )
)
