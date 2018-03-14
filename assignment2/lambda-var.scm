;@file lambda-var.scm
; Hold variadic lambda form processing

;Dependencies:
;- utility.scm

; Of the form (lambda ([var]*) [expr]+)
(define _lambda-var?
    (lambda (expr)
        (and
            ; Check we have a list.
            (list? expr)
            ; Should have "lambda [var] [exp]+" (so 3+ pairs)
            (< 2 (length expr))
            (equal? (car expr) 'lambda)
            ; After lambda there should be a var name (not in a list)
            (_var? (cadr expr))
        )
    )
)

(define parse-lambda-var
    (lambda (expr)
        ; Change to "(lambda-opt () [var] [(parse [exp])]+)
        `(lambda-opt ()
            ; Special form ".[free item]".
            ,(cadr expr)
            ; Expand expressions.
            ,(parse (decompose-list (cddr expr)))
        )
    )
)
