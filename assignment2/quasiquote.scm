;@file quasiquote.scm
; Hold quasiquote processing

(define _quasiquote?
  (lambda (expr)
    (and
      (list? expr)
      (< 0 (length expr))
      ; Expanded automatically to "(quasiquote [expr])".
      (equal? (car expr) 'quasiquote)
    )
  )
)

(define parse-quasiquote
    (lambda (expr)
        ; Use magic function.
        (parse (expand-qq (cadr expr)))
    )
)
