;@file assignment.scm
; Hold assignment (set! ) form processing

;Dependencies:
;- utility.scm

; Of the form (set! [var] [expr]+)
(define _assign?
    (lambda (expr)
        (and
            ; Check we have a list.
            (list? expr)
            ; Should have "(set!" [var] [expr]+)" (so at least 3 elements)
            (< 2 (length expr))
            (equal? (car expr) 'set!)
            (_var? (cadr expr))
        )
    )
)

(define parse-assign
    (lambda (expr)
        ; Change to (set! (var [var]) peExpr)
        `(set
            ; what's being assigned
            ,(parse-var (cadr expr))
            ; Parse expressions
            ,(parse (decompose-list (cddr expr)))
        )
    )
)
