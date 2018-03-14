(define lastSymbol "start_of_data")
(define START_INDEX_CONSTANT 0)


(define indexConstant START_INDEX_CONSTANT)

(define indexConstantIncAndGet
  (lambda ()
    (let ((old indexConstant))
      (set! indexConstant (+ indexConstant 1))
      old
    )
  )
)

(define constantTable `(0 ,*void-object* () ,#t ,#f))

(define resetConstantTable
  (lambda ()
    (set! constantTable `(0 ,*void-object* () ,#t ,#f))
    (set! indexConstant START_INDEX_CONSTANT)
  )
)

(define addConstants
  (lambda (pe)
    (cond
      ( (or
          (not (list? pe))
          (null? pe)
        )
        #f)
      ((equal? (car pe) 'const)
        (cond
          ((vector? (cadr pe))
            (begin
              (vector-map (lambda (el) (addConstants `(const ,el))) (cadr pe))
              (set! constantTable (append constantTable (cdr pe)))))
          ( (and
              (list? (cadr pe))
              (null? (cadr pe)))
            #f)
          ((pair? (cadr pe))
            (begin
              (addConstants `(const ,(caadr pe)))
              (addConstants `(const ,(cdadr pe)))
              (set! constantTable (append constantTable (list (cadr pe))))
            )
          )
          ( (and
              (number? (cadr pe))
              (not (= (/ (denominator (cadr pe)) (gcd (numerator (cadr pe)) (denominator (cadr pe)))) 1))
            )
            (let*
              (
                (highest (gcd (numerator (cadr pe)) (denominator (cadr pe))))
                (numer (/ (numerator (cadr pe)) highest))
                (denom (/ (denominator (cadr pe)) highest))
              )
              (begin
                (addConstants `(const ,numer))
                (addConstants `(const ,denom))
                (set! constantTable (append constantTable (list (cadr pe))))
              )
            )
          )
          ( (string? (cadr pe))
            (set! constantTable (append constantTable (list (cadr pe))))
          )
          ( (symbol? (cadr pe))
            (begin
              (set! constantTable (append constantTable (list (symbol->string (cadr pe)))))
              (set! constantTable (append constantTable (list (cadr pe))))
            )
          )
          (else (set! constantTable (append constantTable (list (cadr pe)))))
        )
      )
      (else (map addConstants pe))
    )
  )
)


(define removeDuplicatesFromConstTable
  (lambda ()
    (set! constantTable (removeDuplicates constantTable))
  )
)

(define removeDuplicatesOfConstTable
  (lambda ()
    (set! constantTable (removeDuplicates constantTable))
  )
)


(define createLabel
  (lambda (el)
    (let*
      (
        (index (car el))
        (c (cadr el))
        (tagVal (caddr el))
        (tag (caaddr el))
      )
      (cond
        ((equal? tag 'T_UNDEFINED)  "sobUnd")
        ((equal? tag 'T_NIL )       "sobNil")
        ((equal? tag 'T_VOID)       "sobVoid")
        ((equal? tag 'T_BOOL)
          (if (equal? c #t)
            "sobTrue"
            "sobFalse"
          )
        )
        ((equal? tag 'T_INTEGER)
          (string-append "sobInt" (number->string index))
        )
        ((equal? tag 'T_CHAR)
          (string-append "sobChr" (number->string index))
        )
        ((equal? tag 'T_PAIR)
          (string-append "sobPair" (number->string index))
        )
        ((equal? tag 'T_FRACTION)
          (string-append "sobFra" (number->string index))
        )
        ((equal? tag 'T_STRING)
          (string-append "sobStr" (number->string index))
        )
        ((equal? tag 'T_SYMBOL)
          (string-append "sobSym" (number->string index))
        )
        ((equal? tag 'T_VECTOR)
          (string-append "sobVec" (number->string index))
        )
        (else (error tag "tag error in createLabel"))
      )
    )
  )
)
(define getLabelFromTable
  (lambda(index)
    (createLabel
      (car
        (filter
          (lambda(el)
            (equal? index (car el))
          )
          constantTable
        )
      )
    )
  )
)
(define getLabelFromTableTest
  (lambda()
    ;(display constantTable)
    (display (getLabelFromTable 2))
  )
)

(define addTagAndIndex
  (lambda()
    (set! constantTable
      (fold-left
        (lambda (newTable c)
          (append newTable
            (list
              (cond
                ((equal? c 0) `(,(indexConstantIncAndGet) ,c (T_UNDEFINED, 0)))
                ((equal? c *void-object*) `(,(indexConstantIncAndGet) ,c (T_VOID, 0)))
                ((equal? c '()) `(,(indexConstantIncAndGet) ,c (T_NIL, 0)))
                ((equal? c #t) `(,(indexConstantIncAndGet) ,#t (T_BOOL, 1)))
                ((equal? c #f) `(,(indexConstantIncAndGet) ,#f (T_BOOL, 0)))
                ( (number? c)
                  (let*
                    ((highest (gcd (numerator c) (denominator c)))
                    (numer (/ (numerator c) highest))
                    (denom (/ (denominator c) highest)))
                    (if (= denom 1)
                      `(,(indexConstantIncAndGet) ,numer (T_INTEGER ,numer))
                      `(,(indexConstantIncAndGet) ,c (T_FRACTION ,(getIndexFromTable numer newTable) ,(getIndexFromTable denom newTable)))
                    )
                  )
                )
                ((char? c) `(,(indexConstantIncAndGet) ,c (T_CHAR ,(char->integer c))))
                ( (vector? c)
                  (let
                    ((addresses (map (lambda (el) (getIndexFromTable el newTable)) (vector->list c))))
                    `(,(indexConstantIncAndGet) ,c (T_VECTOR ,(vector-length c) ,@addresses))
                  )
                )
                ( (string? c)
                  (let
                    ((ascii (map (lambda (el) (char->integer el)) (string->list c))))
                    `(,(indexConstantIncAndGet) ,c (T_STRING ,(string-length c) ,@ascii))
                  )
                )
                ( (symbol? c)
                  (let
                      (
                        (ascii (map (lambda (el) (char->integer el)) (string->list (symbol->string  c))))
                      )
                    `(,(indexConstantIncAndGet) ,c (T_SYMBOL ,(string-length (symbol->string c)) ,(symbol->string c) ,@ascii))
                  )
                )
                  ;`(,(indexConstantIncAndGet) ,c (T_SYMBOL ,(getIndexFromTable (symbol->string c) newTable))))
                ( (pair? c)
                  `(,(indexConstantIncAndGet) ,c (T_PAIR ,(getIndexFromTable (car c) newTable) ,(getIndexFromTable (cdr c) newTable)))
                )
                (else (error 'c "add tag and index error"))
              )
            )
          )
        )
        '()
        constantTable
      )
    )
  )
)


(define generateLabel
  (lambda (el)
    (let
      (
        (index (car el))
        (c (cadr el))
        (tagVal (caddr el))
        (tag (caaddr el))
      )
      (cond
        ((equal? tag  'T_UNDEFINED)
          "sobUnd:\n\t resq 1 \n"
        )
        ((equal? tag 'T_NIL)
          "sobNil:\n\t resq 1 \n"
        )
        ((equal? tag  'T_VOID)
          "sobVoid:\n\t resq 1 \n"
        )
        ((equal? tag 'T_BOOL)
          (if (equal? c #t)
            "sobTrue:\n\t resq 1 \n"
            "sobFalse:\n\t resq 1 \n"
          )
        )
        ( ;T_INTEGER,T_CHAR,T_PAIR,T_FRACTION,T_STRING,T_VECTOR
          (or
            (equal? tag 'T_INTEGER)
            (equal? tag 'T_CHAR)
            (equal? tag 'T_PAIR)
            (equal? tag 'T_FRACTION)
            (equal? tag 'T_STRING)
            (equal? tag 'T_VECTOR)
          )
          (string-append
            (getLabelFromTable index) ":\n\t resq 1 \n"
          )
        )
        ((equal? tag 'T_SYMBOL)
          (string-append
            (getLabelFromTable index) ":\n\t resq 1 \t\nresq 1 \n"
          )
        )
        (else (error tag "tag error"))
      )
    )
  )
)
(define generateAssignment
  (lambda (el)
    (let
      (
        (index (car el))
        (c (cadr el))
        (tagVal (caddr el))
        (tag (caaddr el))
      )
      (cond
        ((equal? tag  'T_UNDEFINED)
          "mov qword[sobUnd], SOB_UNDEFINED \n"
        )
        ((equal? tag 'T_NIL)
          "mov qword[sobNil], SOB_NIL \n"
        )
        ((equal? tag  'T_VOID)
          "mov qword[sobVoid], SOB_VOID \n"
        )
        ((equal? tag 'T_BOOL)
          (if (equal? c #t)
            "mov qword[sobTrue], SOB_TRUE \n"
            "mov qword[sobFalse], SOB_FALSE \n"
          )
        )
        ( ;T_INTEGER,T_CHAR
          (or
            (equal? tag 'T_INTEGER)
            (equal? tag 'T_CHAR)
          )
          (string-append
          "mov qword["(getLabelFromTable index)"], MAKE_LITERAL"(toString tagVal) "\n"
          )
        )
        ((equal? tag 'T_PAIR)
          (let
            (
              (carIndex (cadr tagVal))
              (cdrIndex (caddr tagVal))
            )
            (string-append
              "MAKE_MALLOC_LITERAL_PAIR "
              (getLabelFromTable index) ", "
              (getLabelFromTable carIndex) ", "
              (getLabelFromTable cdrIndex) "\n"
            )
          )
        )
        ((equal? tag 'T_FRACTION)
          (let
            (
              (numerIndex (cadr tagVal))
              (denomIndex (caddr tagVal))
            )
            (string-append
              "MAKE_MALLOC_LITERAL_FRACTION "
              (getLabelFromTable index)", "
              (getLabelFromTable numerIndex) ", "
              (getLabelFromTable denomIndex)"\n"
            )
          )
        )
        ((equal? tag 'T_STRING)
          (let
            (
              (stringVal (cddr tagVal))
              (stringLength (cadr tagVal))
              (strIndex -1)
            )
            (string-append
              "ALLOCATE(" (number->string (+ stringLength 1 )) ")\n"
              ; Insert char by char.
              (fold-left
                (lambda (x r)
                  (begin
                    (set! strIndex (+ strIndex 1))
                    (string-append
                      x
                      "mov qword[rax + " (number->string strIndex) "], \""  (string (integer->char r)) "\"\n"
                    )
                  )
                )
                ""
                stringVal
              )
              ; string length
              "mov rbx, " (number->string stringLength) "\n"
              "sal rbx, 30\n"
              "mov rcx, rax\n"
              "sub rcx, start_of_data\n"
              "or rbx, rcx\n"
              "sal rbx, 4\n"
              "or rbx, T_STRING\n"
              "mov qword["(getLabelFromTable index)"], rbx\n"
            )
          )
        )
        ((equal? tag 'T_VECTOR)
          (let
            (
              (addressesIndex (cddr tagVal))
              (vectorLength (cadr tagVal))
              (vecIndex -1)
            )
            (string-append
              "ALLOCATE(" (number->string (* 8 (+ vectorLength 1 ))) ")\n"
              ; Insert pointer by pointer..
              (fold-left
                (lambda (x r)
                  (begin
                    (set! vecIndex (+ vecIndex 1))
                    (string-append
                      x
                      "mov qword[rax + " (number->string (* 8 vecIndex)) "], "  (getLabelFromTable r) "\n"
                    )
                  )
                )
                ""
                addressesIndex
              )
              ; string length
              "mov rbx, " (number->string vectorLength) "\n"
              "sal rbx, 30\n"
              "mov rcx, rax\n"
              "sub rcx, start_of_data\n"
              "or rbx, rcx\n"
              "sal rbx, 4\n"
              "or rbx, T_VECTOR\n"
              "mov qword["(getLabelFromTable index)"], rbx\n"
            )
          )
        )
        ((equal? tag 'T_SYMBOL)
          (let
            (
              (stringVal (cdddr tagVal))
              (stringLength (cadr tagVal))
              (symbolName (caddr tagVal))
              (strIndex -1)
              (symNumber (getCount 'symbol))
              (last lastSymbol)
            )
            (begin
              (set! lastSymbol (getLabelFromTable index))
              (string-append
                ; Create string.
                "ALLOCATE(" (number->string (+ stringLength 1 )) ")\n"
                ; Insert char by char.
                (fold-left
                  (lambda (x r)
                    (begin
                      (set! strIndex (+ strIndex 1))
                      (string-append
                        x
                        "mov qword[rax + " (number->string strIndex) "], \""  (string (integer->char r)) "\"\n"
                      )
                    )
                  )
                  ""
                  stringVal
                )
                ; save string position.
                "mov rcx, " (number->string stringLength) "\n"
                "sal rcx, 30\n"
                ; string length
                "mov r10, rax\n"
                "sub r10, start_of_data\n"
                "or rcx, r10\n"
                "sal rcx, 4\n"
                "or rcx, T_STRING\n"
                ; Save actual string wrapper.
                "ALLOCATE(8)\n"
                "mov qword[rax], rcx\n"
                "mov rbx, rax\n"
                "sub rbx, start_of_data\n"
                ; Create symbol.
                "sal rbx, 4\n"
                "or rbx, T_SYMBOL\n"
                "mov qword["(getLabelFromTable index)"], rbx\n"
                "mov qword["(getLabelFromTable index)" + 8], "last "\n"
              )
            )
          )
        )
        (else (error tag "tag error"))
      )
    )
  )
)


(define getConstTableInitialiStr

  (lambda ()
    (fold-left
      string-append
      ""
      (cMap generateLabel constantTable)
    )
  )
)
(define getConstTableAssignmentsStr
  (lambda ()
    (fold-left
      string-append
      ""
      (cMap generateAssignment constantTable)
    )
  )
)
(define generateConstTable
  (lambda (parsedFile)
    (resetConstantTable)
    (addConstants parsedFile)
    (removeDuplicatesOfConstTable)
    (addTagAndIndex)
    ;(getConstantTableStr)
  )
)
