;@file seq.scm
; Hold seq (expression sequence) form processing

;Dependencies:
;- utility.scm

; Of the form ([expr][expr]+)
(define _seq?
    (lambda (expr)
        (and
            ; Check we have a list.
            (list? expr)
            ; Should have "[expre] [expre]+" (so at least 2 elements)
            (< 0 (length expr))
            (equal? (car expr) 'begin)
        )
    )
)

(define parse-seq
    (lambda (expr)
        ; Change to (seq [([peExpr)]+))
        `(seq
            ,(map parse (cdr expr))
        )
    )
)
