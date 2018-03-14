;@file lambda-simple.scm
; Hold simple lambda form processing

;Dependencies:
;- utility.scm

; Of the form (lambda ([var]*) [expr]+)
(define _lambda-simple?
    (lambda (expr)
        (and
            ; Check we have a list.
            (list? expr)
            ; Should have "lambda" ([var]*) [expr]+ (so at least 3 elements)
            (< 2 (length expr))
            (equal? (car expr) 'lambda)
            ; Next - check we have a list of variables (can be empty, but needs a list)
            (list? (cadr expr))
            ; Check legal variable name
            (check-var-names (cadr expr))
            ; Check we don't have double variables
            (check-no-double-vars (cadr expr))
        )
    )
)

(define parse-lambda-simple
    (lambda (expr)
        ; Change to "lambda-simple" ([var]*) (parse [exp])
        `(lambda-simple ,(cadr expr)
            ,(parse (decompose-list (cddr expr)))
        )
    )
)
