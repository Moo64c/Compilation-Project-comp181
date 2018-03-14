;@file const.scm
; Hold constant processing

(define _const?
  (lambda (expr)
    (or
      (null? expr)
      (boolean? expr)
      (number? expr)
      (char? expr)
      (string? expr)
      (quote? expr)
      (vector? expr)
    )
  )
)

(define parse-quote
    (lambda (q)
        `(const ,(cadr q))
    )
)

(define parse-const
    (lambda (expr)
        (if (quote? expr)
          (parse-quote expr)
        ;else 
          `(const ,expr)
        )
    )
)
