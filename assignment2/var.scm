;@file var.scm
; Hold var processing

(define __reserved-words
    '(and begin cond define do else if lambda let let* letrec or quasiquote
        unquote unquote-splicing quote set!))

(define _reserved?
    (letrec (
        (in-list?
            (lambda (needle haystack)
                (cond
                    ((null? haystack)
                        #f)
                    ((equal? (car haystack) needle)
                        #t)
                    (else
                        (in-list? needle (cdr haystack)))
                )
            )
        )
    )
    (lambda (expr)
        (in-list? expr __reserved-words)
    ))
)

(define _var?
    (lambda (expr)
        (and
            (symbol? expr)
            (not (_reserved? expr))
        )
    )
)

(define parse-var
    (lambda (expr)
        `(var ,expr)
    )
)
