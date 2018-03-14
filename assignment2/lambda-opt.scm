;@file lambda-opt.scm
; Hold optional-params lambda form processing

;Dependencies:
;- utility.scm

; Of the form (lambda ([var]*) [expr]+)
(define _lambda-opt?
    (letrec (
        (improper-list-of-vars?
            (lambda (_list)
                (cond
                    ((null? (cdr _list))
                            #f)
                    ((and
                        (_var? (car _list))
                        (_var? (cdr _list))
                        )
                            #t)
                    ((and
                        (_var? (car _list))
                        (_var? (cadr _list))
                        )
                            (improper-list-of-vars? (cdr _list)))
                    (else
                            #f))
            )
        )
    )
    (lambda (expr)
        (and
            ; Check we have a list.
            (list? expr)
            ; Should have "lambda" ([var]* . [var] [expr]+ (so at least 3 elements)
            (< 2 (length expr))
            (equal? (car expr) 'lambda)
            (pair? (cadr expr))
            ; Check this is an improper list of variables
            (improper-list-of-vars? (cadr expr))
        )
    ))
)

(define parse-lambda-opt
    (letrec (
        ; Last item (free argument) should be a legit var.
        (all-except-last
            (lambda (_list)
                (if (_var? (cdr _list))
                    ; Next item is the .[free item].
                    (list (car _list))
                    (cons (car _list) (all-except-last (cdr _list))))))
        (last-in-list
            (lambda (_list)
                (if (_var? (cdr _list))
                    (cdr _list)
                    (last-in-list (cdr _list))))))

    (lambda (expr)
        ; Change to "lambda-opt" ([var]*) [opt] (parse [exp])
        `(lambda-opt
            ; List of normal parameters.
            ,(all-except-last (cadr expr))
            ; Special form ".[free item]".
            ,(last-in-list (cadr expr))
            ,(parse (decompose-list (cddr expr)))
        )
    ))
)
