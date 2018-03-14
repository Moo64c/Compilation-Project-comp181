(define is-var-or-const?
  (lambda (expr)
    (and
      (list? expr)
      (< 0 (length expr))
      (or
        (equal? 'var (car expr))
        (equal? 'const (car expr))
      )
    )
  )
)

(define is-if3?
  (lambda (expr)
    (and
      (list? expr)
      (< 0 (length expr))
      (equal? 'if3 (car expr))
    )
  )
)

(define is-seq?
  (lambda (expr)
    (and
      (list? expr)
      (< 0 (length expr))
      (equal? 'seq (car expr))
    )
  )
)

(define is-def?
  (lambda (expr)
    (and
      (list? expr)
      (< 0 (length expr))
      (equal? 'def (car expr))
    )
  )
)

(define is-lambda?
  (lambda (expr)
    (and
      (list? expr)
      (< 0 (length expr))
      (or
        (equal? 'lambda-simple (car expr))
        (equal? 'lambda-opt (car expr))
        (equal? 'lambda-var (car expr))
      )
    )
  )
)

(define is-applic?
  (lambda (expr)
    (and
      (list? expr)
      (< 0 (length expr))
      (equal? 'applic (car expr))
    )
  )
)

(define or?
  (lambda (sxpr)
    (and
      (list? sxpr)
      (< 0 (length sxpr))
      (equal? 'or (car sxpr))
    )
  )
)
