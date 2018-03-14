;@file applic.scm
; Hold applic (apply function to var) form processing

;Dependencies:
;- utility.scm

; Of the form ([var] [var]*)
(define _applic?
    (lambda (expr)
        (and
            ; Check we have a list.
            (list? expr)
            ; Should have "([var] [var]*)" (so at least 1 element)
            (< 0 (length expr))
            ; First one is the function.. could be anything like a result of
            ; another applic (just not a reserved word)
            (not (_reserved? (car expr)))
        )
    )
)

(define parse-applic
    (lambda (expr)
        ; Change to (apllic (var [var]) ([var]*)))
        `(applic
            ; Do the first var
            ,(parse (car expr))
            ; /r/restofthefuckingowl
            ,(map parse (cdr expr))
        )
    )
)
