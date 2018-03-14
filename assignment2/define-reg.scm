;@file define-reg.scm
; Hold regular define form processing

;Dependencies:
;- utility.scm

; Of the form (define [var] [expr]+)
(define _define-reg?
    (lambda (expr)
        (and
            ; Check we have a list.
            (list? expr)
            ; Should have "(define" [var] [expr]+)" (so at least 3 elements)
            (< 2 (length expr))
            (equal? (car expr) 'define)
            (_var? (cadr expr))
        )
    )
)

(define parse-define-reg
    (lambda (expr)
        ; Change to (define (var [var] peExpr))
        `(define
            ; what's being defined
            ,(parse-var (cadr expr))
            ; Parse expressions
            ,(parse (decompose-list (cddr expr)))
        )
    )
)
