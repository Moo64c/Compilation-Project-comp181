(define *void-object*
  (if #f #f)
)
(define counters '())

; Saves a list of counters to save usages.
(define getCount
  (lambda (tag-name)
    (let
      (
        (data (assoc tag-name counters))
      )
      (if data
        (number->string (begin
          ; Increment!
          (set-box!
            (cdr data)
            (+
              (unbox (cdr data)) 1)
          )
          ; Return new value.
          (unbox (cdr data))
        ))
        (begin
          ; Create new data pair for the tag name.
          (set!
            counters
            (append
              counters
              (list (cons tag-name (box 0))))
          )
          ; Get new count (start from 1...)
          (getCount tag-name)
        )
      )
    )
  )
)

(define removeDuplicates
  (lambda (lst)
    (fold-left
      (lambda (acc el)
        (if (member el acc)
          acc
          (append acc (list el)))
      )
      '()
      lst
    )
  )
)
(define cMap
  (lambda (proc lst)
    (fold-left
      (lambda (acc el)
        (append acc (list (proc el)))
      )
      '()
      lst
    )
  )
)

(define toString
  (lambda (c)
    (cond
      ((number? c) (number->string c))
      ((symbol? c) (symbol->string c))
      ((boolean? c) (if c "#t" "#f"))
      ((list? c)

        (string-append
          "("
          (fold-left
            (lambda(acc el)
              (if (equal? acc "")
              (toString el)
              (string-append acc ", " (toString el))
              )
            )
            ""
            c
          )
          ")"
        )

      )
      (else c)
    )
  )
)
(define getIndexFromTable
  (lambda (i taggedTable)
    (caar
      (filter
        (lambda (el)
          (equal? i (cadr el))
        )
        taggedTable
      )
    )
  )
)
