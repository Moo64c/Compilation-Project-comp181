;@file define-mit.scm
; Hold mit define form processing

;Dependencies:
;- utility.scm

; Of the form (define ([var] [var]+) [expr]+)
(define _define-mit?
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
            ; Should have "(define" ([var]+) [expr]+)" (so at least 3 elements)
            (< 2 (length expr))
            (equal? (car expr) 'define)
            (or
                (and
                    ; Lambda - simple
                    (list? (cadr expr))
                    (< 0 (length (cadr expr)))
                    (check-var-names (cadr expr))
                )
                (and
                    ; Lambda opt
                    (pair? (cadr expr))
                    (improper-list-of-vars? (cadr expr))
                )
            )
        )
    ))
)

(define parse-define-mit
    (lambda (expr)
        ; Change to (define (var [var]) ([lambdaExpre])
        `(define
            ; what's being defined
            ,(parse-var (car (cadr expr)))
            ; Parse expressions by converting to lambda form.
            ,(parse `(lambda ,(cdr (cadr expr)) ,(decompose-list (cddr expr))))
        )
    )
)
