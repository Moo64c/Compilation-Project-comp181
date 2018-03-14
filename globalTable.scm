(define START_INDEX_GLOBAL 1)

(define indexGlobal START_INDEX_GLOBAL)

(define indexGlobalIncAndGet
  (lambda ()
    (let ((old indexGlobal))
      (set! indexGlobal (+ indexGlobal 1))
      old
    )
  )
)

(define globalLibraryFunctions
  '(bin-bigger bin-equal bin-append cons car cdr map append fold-left
    + apply null? pair? boolean? char? integer? procedure? string? symbol?
    vector? make-string make-vector not list remainder fraction? string-length vector-length zero? vector
    char->integer integer->char - * -div -equal -bigger -smaller vector-ref
    number? rational?
    symbol->string
    string-ref
    eq? string-set!
    numerator denominator
   )
)
(define globalLibraryWeirdNamesFixes
  (string->list
    (string-append
      "(define / -div)\n"
      "(define = -equal)\n"
      "(define > -bigger)\n"
      "(define < -smaller)\n"
    )
  )
)
;(define globalLibraryFunctions2
;  '(bin-bigger bin-equal bin-mul bin-minus cons car cdr map append + apply null? pair? boolean? char? integer? procedure?
;      string? symbol? vector? make-string make-vector not string-length vector-length zero? vector char->integer
;      integer->char string-ref vector-ref symbol->string string->symbol eq? - * / = > < expt
;      + list remainder denominator numerator number? rational? string-set! vector-set! )
;)
(define getLabelFvar
  (lambda (c)
    (let
      ((index (getIndexFromTable c globalTable)))
      (string-append "fvar_" (number->string index))
    )
  )
)
(define globalTable globalLibraryFunctions)
(define resetGlobalTable
  (lambda ()
    (set! globalTable globalLibraryFunctions)
  )
)
(define addFvars
  (lambda (pe)
    (cond
      ((or (not (list? pe))(null? pe))
        #f)
      ((equal? (car pe) 'fvar)
        (set! globalTable (append globalTable (list (cadr pe))))
      )
      (else (cMap addFvars pe))
    )
  )
)
(define removeDuplicatesOfglobalTable
  (lambda ()
    (set! globalTable (removeDuplicates globalTable))
  )
)
(define addIndexFvar
  (lambda ()
    (set! globalTable
      (fold-left
        (lambda (newTable c)
          (append newTable (list `(,(indexGlobalIncAndGet) ,c 0xDEF)))
        )
        '()
        globalTable
      )
    )
  )
)
(define getGlobalTableInitialiStr
  (lambda ()
    (fold-left
      string-append
      ""
      (cMap
        (lambda (row)
          (string-append
              ";fvar "
              (toString(cadr row))
              "\n"
              (string-append
                (getLabelFvar (cadr row)) ":\n \t resq 1 \n"
              )

          )
        )
        globalTable
      )
    )
  )
)
(define getGlobalTableAssignmentsStr
  (lambda ()
    (fold-left
      string-append
      ""
      (cMap
        (lambda (row)
          (if (member (cadr row)  globalLibraryFunctions)
            (string-append
              "call start_" (getLabelFvar (cadr row)) "\n"
            )
            ""
          )
        )
        globalTable
      )
    )
  )
)
(define generateGlobalTable
  (lambda (parsedFile)
    (resetGlobalTable)
    (addFvars parsedFile)
    (removeDuplicatesOfglobalTable)
    (addIndexFvar)
    ;(display globalTable)

  )
)
