  ;; Created by Amir Arbel and Ronen Finish
;; @file genLibraryFunctions.scm
;; Contains code generation functions for:
;append (variadic),apply(not variadic),<(variadic), = (variadic),
;>(variadic),+(variadic),/(variadic),(variadic),-(variadic),
;boolean?,car,cdr,char->integer,char?,cons,denominator,eq?,integer?,integer->char,
;list(variadic),make-string,make-vector,map(variadic),not,null?,
;number?,numerator,pair?,procedure?,rational?,remainder
;string-length,string-ref,string-set!,string->symbol,
;string?,symbol?,symbol->string,vector,vector-length,vector-ref,
;vector-set!,vector?,zero?

(define genLibraryFunctions
  (lambda ()
    (string-append
      (genCons)
      (genCar)
      (genCdr)
      (genMap)
      (genBinAppend)
      (genAppend)
      (genNull?)
      (genPair?)
      (genBoolean?)
      (genChar?)
      (genInteger?)
      (genProcedure?)
      (genString?)
      (genSymbol?)
      (genVector?)
      (genFraction?)
      (genApply)
      (genMakeString);TODO
      (genMakeVector)
      (genNot)
      (genStringLength)
      (genVectorLength)
      (genZero?)
      (genVector)
      (genCharToInteger)
      (genIntegerToChar)
      (genString-ref)
      (genVectorRef)
      (genSymbol->string)
      ;(genString->symbol);TODO
      (genList)
      (genMul)
      (genDivide)
      (genMinus)
      (genBinEqual)
      (genEqual)
      (genBinBigger)
      (genSmallerThan)
      (genBiggerThan)
      (genSetString)
      ;(genSetVector);TODO
      (genPlus)
      (genNumerator)
      (genDenominator)
      (genNumber?)
      (genRational?)
      (genEq?)
      (genRemainder)
      (genFoldLeft)
    )
  )
)

(define genNull?
  (lambda ()
    (checkTypeGen 'null? "T_NIL")
  )
)
(define genInteger?
  (lambda ()
    (checkTypeGen 'integer? "T_INTEGER")
  )
)
(define genProcedure?
  (lambda ()
    (checkTypeGen 'procedure? "T_CLOSURE")
  )
)
(define genFraction?
  (lambda ()
    (checkTypeGen 'fraction? "T_FRACTION")
  )
)
(define genBoolean?
  (lambda ()
    (checkTypeGen 'boolean? "T_BOOL")
  )
)
(define genChar?
  (lambda ()
    (checkTypeGen 'char? "T_CHAR")
  )
)
(define genString?
  (lambda ()
    (checkTypeGen 'string? "T_STRING")
  )
)
(define genSymbol?
  (lambda ()
   (checkTypeGen 'symbol? "T_SYMBOL ")
  )
)
(define genPair?
  (lambda ()
    (checkTypeGen 'pair? "T_PAIR")
  )
)
(define genVector?
  (lambda ()
    (checkTypeGen 'vector? "T_VECTOR")
  )
)
(define genNumber?
  (lambda ()
    (start_scheme_library_functions
      'number?
      "(define number?
        (let((int? integer?)(fra? fraction?))
          (lambda (x)
            (or (int? x) (fra? x))
          )
        )
      )"
    )
  )
)
(define genRational?
  (lambda ()
    (start_scheme_library_functions
      'rational?
      "(define rational?
        (let((num? number?))
          (lambda (x)
            (num? x)
          )
        )
      )"
    )
  )
)
(define genZero?
  (lambda ()
    (start_scheme_library_functions
      'zero?
      "(define zero?
        (let((equ bin-equal))
          (lambda (num)
            (equ 0 num)
          )
        )
      )"
    )
  )
)
(define genEq?
  (lambda ()
    (start_scheme_library_functions
      'eq?
      "(define eq?
        (let((equ bin-equal))
          (lambda (x y)
            #f
          )
        )
      )"
    )
  )
)
(define genEqual
  (lambda ()
    (start_scheme_library_functions
      '-equal
      "(define -equal
        (let((equ bin-equal))
          (lambda x
            (if (null? (cdr x)) #t
              (if (equ (car x) (car (cdr x)))
                (apply -equal (cdr x))
                #f
              )
            )
          )
        )
      )"
    )
  )
)
(define genNot
  (lambda ()
    (start_scheme_library_functions
      'not
      "(define not
        (lambda (value)
          (if value
            #f
            #t
          )
        )
      )"
    )
  )
)
(define genMap
  (lambda ()
    (start_scheme_library_functions
      'map
      "(define map
        (lambda (proc lst)
          (if (null? lst)
            '()
            (cons (proc (car lst)) (map proc (cdr lst)))
          )
        )
      )"
    )
  )
)
(define genFoldLeft
  (lambda ()
    (start_scheme_library_functions
      'fold-left
      "(define fold-left
        (lambda (proc init lst)
          (cond
            ((null? lst)
              init
            )
            (else
              (fold-left proc (proc init (car lst)) (cdr lst))
            )
          )
        )
      )"
    )
  )
)
(define genBiggerThan
  (lambda ()
    (start_scheme_library_functions
      '-bigger
      "(define -bigger
        (let((big bin-bigger))
          (lambda x
            (if (null? (cdr x)) #t
              (if (big (car x) (car (cdr x)))
                (apply -bigger (cdr x))
                #f
              )
            )
          )
        )
      )"
    )
  )
)
(define genSmallerThan
  (lambda()
    (start_scheme_library_functions
      '-smaller
      "(define -smaller
        (let((big bin-bigger))
          (lambda x
            (if (null? (cdr x)) #t
              (if (big (car (cdr x)) (car x))
                (apply -smaller (cdr x))
                #f
              )
            )
          )
        )
      )"
    )
  )
)
(define genBinAppend
  (lambda ()
    (start_scheme_library_functions 'bin-append
      "(define bin-append
        (lambda (lst1 lst2)
          (if (null? lst1)
      			lst2
      			(cons (car lst1) (bin-append (cdr lst1) lst2))
          )
        )
      )"
    )
  )
)
(define genAppend
  (lambda ()
    (start_scheme_library_functions
      'append
      "(define append
        (lambda x (fold-left bin-append '() x))
      )"
    )
  )
)
(define genList
  (lambda()
    (start_scheme_library_functions
      'list
      "(define list (lambda x x))"
    )
  )
)
(define genPlus
  (lambda ()
    (string-append
      (start_assembly_library_functions '+)
      (getLabelFvar '+) "_body:\n"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      ; rbx - Numerator.
      "mov rbx, 0\n\t"
      ; rcx - Denominator
      "mov rcx, 1 \n\t"
      ; r8 - number of args.
      "mov r8, qword[rbp+3*8]\n\t"
      ; if we have 0 args, return 0
      "cmp r8, 0\n"
      "jne L_plus_continue\n"
      "ALLOCATE(8)\n"
      "mov rbx, 0 \n\t"
      "sal rbx, TYPE_BITS \n\t"
      "or rbx, T_INTEGER \n\t"
      "mov qword[rax], rbx\n"
      "jmp L_plus_end_create\n"
      "L_plus_continue:\n"
      ; r9 - Loop counter.
      "mov r9, 0 \n\t"
      ;; if more than 1 arg we dont start with 0
      "cmp r8, 1 \n"
      "je L_plus_frac_done \n\t"
      ; Increment counter.
      "add r9, 1 \n\t"
      ; First argument => rbx.
      "mov rbx, qword [rbp+4*8] \n\t"
      "mov rbx, [rbx] \n\t"

      ;; Check type.
      "mov rcx, rbx  \n\t"
      "TYPE rcx \n\t"
      "cmp rcx, T_FRACTION \n\t"
      "je L_plus_frac \n\t"
      "mov rcx, 1 \n\t"
      "DATA rbx \n\t"
      "jmp L_plus_frac_done \n\t"
      "L_plus_frac: \n\t\t"
      "mov rcx, rbx \n\t\t"
      "CAR rbx \n\t\t"
      "DATA rbx\n"
      "CDR rcx \n\t"
      "DATA rcx\n"

      "L_plus_frac_done:\n\t"
      "L_plus_loop:\n\t"
      "cmp r9, r8\n\t"
      "je L_end_plus_loop\n\t"
      "lea r13, [rbp + (r9+4)*8]\n\t"
      ;; rax := next arg
      "mov rax, qword[r13]  \n\t"
      "mov rax, [rax]\n\t"
      ;; check if arg is int or frac
      "mov rdx, rax  \n\t"
      "TYPE rdx \n\t"
      "cmp rdx, T_FRACTION \n\t"
      "je L_plus_make_frac \n\t"
      ; rax is an integer
      "mov rdx, 1 \n\t"
      "DATA rax \n\t"
      "jmp L_calculate_plus \n\t"
      "L_plus_make_frac: \n\t"
      "mov rdx, rax \n\t"
      "CAR rax \n\t"
      "DATA rax\n"
      "CDR rdx \n\t"
      "DATA rdx\n\t"
      "L_calculate_plus:\n\t"
      "mov r12, rax\n"
      "mov r13, rdx\n"
      "mov r14, rbx\n"
      "mov r15, rcx\n"
      "PLUS  r14, r15, r12, r13 \n\t"
      "mov rax, r12 \n"
      "mov rdx, r13 \n"
      "mov rbx, r14 \n"
      "mov rcx, r15 \n"

      "add r9, 1 \n\t"
      "jmp L_plus_loop \n\t"
      "L_end_plus_loop:\n\t"
      "REDUCER r10, r11 \n\t"

      "cmp r11, 1 \n\t"
      "jne L_plus_create_frac \n\t"
      ; Create integer.
      "ALLOCATE(8)\n"
      "mov rbx, r10 \n\t"
      "sal rbx, TYPE_BITS \n\t"
      "or rbx, T_INTEGER \n\t"
      "mov qword[rax], rbx\n"
      "jmp L_plus_end_create \n\t"

      "L_plus_create_frac:\n\t"
      ;Create fraction
      "ALLOCATE(8)\n"
      "sal r10, 4\n"
      "or r10, T_INTEGER\n"
      "mov qword[rax], r10\n"
      "mov r10, rax\n"
      "ALLOCATE(8)\n"
      "sal r11, 4\n"
      "or r11, T_INTEGER\n"
      "mov qword[rax], r11\n"
      "mov r11, rax\n"
      "ALLOCATE(8)\n"
      "MAKE_MALLOC_LITERAL_FRACTION rax, r10, r11\n"
      "L_plus_end_create: \n\t"

      ; rax holds result
      "leave\n\t"
      "ret\n\t"
    )
  )
)

(define genRemainder
 	(lambda()
		(string-append
      (start_assembly_library_functions 'remainder)
      (getLabelFvar 'remainder) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      "mov rbx, arg_count\n\t"

      "mov rax, An(0)\n\t"
      "mov rax, [rax]\n\t"
      "mov r11, rax\n\t"
      "mov rbx, An(1)\n\t"
      "mov rbx, [rbx]\n\t"
      "mov r10, rbx\n\t"
      "DATA rax\n\t"
      "DATA rbx\n\t"
      "mov r11, rax\n\t"
      "mov r10, rbx\n\t"
      "cmp r11, 0\n\t"
      "jge is_not_negative1\n\t"
      "neg rax\n\t"
      "is_not_negative1:\n\t\t"
      "mov rdx, qword 0\n\t"
      "idiv rbx\n\t"
      "cmp r11, 0\n\t"
      "jge is_not_negative2\n\t"
      "neg rdx\n\t"
      "is_not_negative2:\n\t\t"
      "sal rdx, TYPE_BITS\n\t\t"
      "add rdx, T_INTEGER\n\t\t"
      "push rdx\n\t\t"
      "ALLOCATE(8)\n\t\t"
      "pop rdx\n\t\t"
      "mov [rax], rdx\n\t\t"

      "leave\n\t\t"
      "ret\n\t\t"
    )
  )
)

(define genMinus
  (lambda ()
    (string-append
      (start_assembly_library_functions '-)
      (getLabelFvar '-) "_body:\n"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      ; rbx - Numerator.
      "mov rbx, 0\n\t"
      ; rcx - Denominator
      "mov rcx, 1 \n\t"
      ; r9 - Loop counter.
      "mov r9, 0 \n\t"
      ; r8 - number of args.
      "mov r8, qword[rbp+3*8]\n\t"
      ;; if more than 1 arg we dont start with 0
      "cmp r8, 1 \n"
      "je L_minus_frac_done \n\t"
      ; Increment counter.
      "add r9, 1 \n\t"
      ; First argument => rbx.
      "mov rbx, qword [rbp+4*8] \n\t"
      "mov rbx, [rbx] \n\t"

      ;; Check type.
      "mov rcx, rbx  \n\t"
      "TYPE rcx \n\t"
      "cmp rcx, T_FRACTION \n\t"
      "je L_minus_frac \n\t"
      "mov rcx, 1 \n\t"
      "DATA rbx \n\t"
      "jmp L_minus_frac_done \n\t"
      "L_minus_frac: \n\t\t"
      "mov rcx, rbx \n\t\t"
      "CAR rbx \n\t\t"
      "DATA rbx\n"
      "CDR rcx \n\t"
      "DATA rcx\n"

      "L_minus_frac_done:\n\t"
      "L_minus_loop:\n\t"
      "cmp r9, r8\n\t"
      "je L_end_minus_loop\n\t"
      "lea r13, [rbp + (r9+4)*8]\n\t"
      ;; rax := next arg
      "mov rax, qword[r13]  \n\t"
      "mov rax, [rax]\n\t"
      ;; check if arg is int or frac
      "mov rdx, rax  \n\t"
      "TYPE rdx \n\t"
      "cmp rdx, T_FRACTION \n\t"
      "je L_minus_make_frac \n\t"
      ; rax is an integer
      "mov rdx, 1 \n\t"
      "DATA rax \n\t"
      "jmp L_calculate_minus \n\t"
      "L_minus_make_frac: \n\t"
      "mov rdx, rax \n\t"
      "CAR rax \n\t"
      "DATA rax\n"
      "CDR rdx \n\t"
      "DATA rdx\n"
      "L_calculate_minus:\n\t"
      "mov r12, rax\n"
      "mov r13, rdx\n"
      "mov r14, rbx\n"
      "mov r15, rcx\n"
      "MINUS r14, r15, r12, r13 \n\t"
      "mov rax, r12 \n"
      "mov rdx, r13 \n"
      "mov rbx, r14 \n"
      "mov rcx, r15 \n"

      "add r9, 1 \n\t"
      "jmp L_minus_loop \n\t"
      "L_end_minus_loop:\n\t"
      "REDUCER r10, r11 \n\t"

      "cmp r11, 1 \n\t"
      "jne L_minus_create_frac \n\t"
      ; Create integer.
      "ALLOCATE(8)\n"
      "mov rbx, r10 \n\t"
      "sal rbx, TYPE_BITS \n\t"
      "or rbx, T_INTEGER \n\t"
      "mov qword[rax], rbx\n"
      "jmp L_minus_end_create \n\t"

      "L_minus_create_frac:\n\t"
      ;Create fraction
      "ALLOCATE(8)\n"
      "sal r10, 4\n"
      "or r10, T_INTEGER\n"
      "mov qword[rax], r10\n"
      "mov r10, rax\n"
      "ALLOCATE(8)\n"
      "sal r11, 4\n"
      "or r11, T_INTEGER\n"
      "mov qword[rax], r11\n"
      "mov r11, rax\n"
      "ALLOCATE(8)\n"
      "MAKE_MALLOC_LITERAL_FRACTION rax, r10, r11\n"
      "L_minus_end_create: \n\t"

      ; rax holds result
      "leave\n\t"
      "ret\n\t"
    )
  )
)
(define genDivide
  (let
    (
      (label-prefix "L_divide_")
    )
    (lambda ()
      (string-append
        (start_assembly_library_functions '-div)
        (getLabelFvar '-div) "_body:\n"
        "push rbp\n\t"
        "mov rbp, rsp\n\t"
        ; rbx - Numerator.
        "mov rbx, 0\n\t"
        ; rcx - Denominator
        "mov rcx, 1 \n\t"
        ; r8 - number of args.
        "mov r8, qword[rbp+3*8]\n\t"
        ; r9 - Loop counter.
        "mov r9, 0 \n\t"
        ;; if more than 1 arg we dont start with 0
        "cmp r8, 1 \n"
        "je " label-prefix "frac_done \n\t"
        ; Increment counter.
        "add r9, 1 \n\t"
        ; First argument => rbx.
        "mov rbx, qword [rbp+4*8] \n\t"
        "mov rbx, [rbx] \n\t"

        ;; Check type.
        "mov rcx, rbx  \n\t"
        "TYPE rcx \n\t"
        "cmp rcx, T_FRACTION \n\t"
        "je "label-prefix"frac \n\t"
        "mov rcx, 1 \n\t"
        "DATA rbx \n\t"
        "jmp "label-prefix"frac_done \n\t"
        ""label-prefix"frac: \n\t\t"
        "mov rcx, rbx \n\t\t"
        "CAR rbx \n\t\t"
        "DATA rbx\n"
        "CDR rcx\n\t"
        "DATA rcx\n"

        ""label-prefix"frac_done:\n\t"
        ""label-prefix"loop:\n\t"
        "cmp r9, r8\n\t"
        "je "label-prefix"end_loop\n\t"
        "lea r13, [rbp + (r9+4)*8]\n\t"
        ;; rax := next arg
        "mov rax, qword[r13]  \n\t"
        "mov rax, [rax]\n\t"
        ;; check if arg is int or frac
        "mov rdx, rax  \n\t"
        "TYPE rdx \n\t"
        "cmp rdx,T_FRACTION \n\t"
        "je "label-prefix"make_frac \n\t"
        ; rax is an integer
        "mov rdx, 1 \n\t"
        "DATA rax \n\t"
        "jmp "label-prefix"calculate \n\t"
        ""label-prefix"make_frac: \n\t"
        "mov rdx, rax \n\t"
        "CAR rax \n\t\t"
        "DATA rax\n"
        "CDR rdx \n\t"
        "DATA rdx\n\t"
        ""label-prefix"calculate:\n\t"
        "mov r12, rax\n"
        "mov r13, rdx\n"
        "mov r14, rbx\n"
        "mov r15, rcx\n"
        "DIVIDE  r14, r15, r12, r13 \n\t"
        "mov rax, r12 \n"
        "mov rdx, r13 \n"
        "mov rbx, r10 \n"
        "mov rcx, r11 \n"

        "add r9, 1 \n\t"
        "jmp "label-prefix"loop \n\t"
        ""label-prefix"end_loop:\n\t"

        "cmp r11, 0\n"
        "jge "label-prefix"after_denom_check\n"
        "neg r11\n"
        "neg r10\n"
        ""label-prefix"after_denom_check:\n"
        "REDUCER r10, r11 \n\t"

        "cmp r11, 1 \n\t"
        "jne "label-prefix"create_frac \n\t"
        ; Create integer.
        "ALLOCATE(8)\n"
        "mov rbx, r10 \n\t"
        "sal rbx, TYPE_BITS \n\t"
        "or rbx, T_INTEGER \n\t"
        "mov qword[rax], rbx\n"
        "jmp "label-prefix"create_end \n\t"

        ""label-prefix"create_frac:\n\t"
        ;Create fraction

        "ALLOCATE(8)\n"
        "sal r10, 4\n"
        "or r10, T_INTEGER\n"
        "mov qword[rax], r10\n"
        "mov r10, rax\n"
        "ALLOCATE(8)\n"
        "sal r11, 4\n"
        "or r11, T_INTEGER\n"
        "mov qword[rax], r11\n"
        "mov r11, rax\n"
        "ALLOCATE(8)\n"
        "MAKE_MALLOC_LITERAL_FRACTION rax, r10, r11\n"
        ""label-prefix"create_end: \n\t"

        ; rax holds result
        "leave\n\t"
        "ret\n\t"
      )
    )
  )
)
(define genMul
  (let
    (
      (label-prefix "L_mul_")
    )
    (lambda ()
      (string-append
        (start_assembly_library_functions '*)
        (getLabelFvar '*) "_body:\n"
        "push rbp\n\t"
        "mov rbp, rsp\n\t"
        ; rbx - Numerator.
        "mov rbx, 0\n\t"
        ; rcx - Denominator
        "mov rcx, 1 \n\t"
        ; r8 - number of args.
        "mov r8, qword [rbp+3*8]\n\t"
        ; if we have 0 args, return 1
        "cmp r8, 0\n"
        "jne " label-prefix "continue\n"
        "ALLOCATE(8)\n"
        "mov rbx, 1 \n\t"
        "sal rbx, TYPE_BITS \n\t"
        "or rbx, T_INTEGER \n\t"
        "mov qword[rax], rbx\n"
        "jmp " label-prefix "create_end\n"
        label-prefix "continue:\n"
        "cmp r8, 1\n"
        "jne " label-prefix "continue2\n"
        "mov rbx, [rbp+4*8] \n\t"
        "mov qword[rax], rbx\n"
        "jmp " label-prefix "create_end\n"
        label-prefix "continue2:\n"
        ; r9 - Loop counter.
        "mov r9, 0 \n\t"
        ;; if more than 1 arg we dont start with 0
        "cmp r8, 1 \n"
        "je " label-prefix "frac_done \n\t"
        ; Increment counter.
        "add r9, 1 \n\t"
        ; First argument => rbx.
        "mov rbx, qword [rbp+4*8] \n\t"
        "mov rbx, [rbx] \n\t"

        ;; Check type.
        "mov rcx, rbx  \n\t"
        "TYPE rcx \n\t"
        "cmp rcx, T_FRACTION \n\t"
        "je "label-prefix"frac \n\t"
        "mov rcx, 1 \n\t"
        "DATA rbx \n\t"
        "jmp "label-prefix"frac_done \n\t"
        ""label-prefix"frac: \n\t\t"
        "mov rcx, rbx \n\t\t"
        "CAR rbx \n\t\t"
        "DATA rbx\n"
        "CDR rcx \n\t"
        "DATA rcx\n"

        ""label-prefix"frac_done:\n\t"
        ""label-prefix"loop:\n\t"
        "cmp r9, r8\n\t"
        "je "label-prefix"end_loop\n\t"
        "lea r13, [rbp + (r9+4)*8]\n\t"
        ;; rax := next arg
        "mov rax, qword[r13]  \n\t"
        "mov rax, [rax]\n\t"
        ;; check if arg is int or frac
        "mov rdx, rax  \n\t"
        "TYPE rdx \n\t"
        "cmp rdx,T_FRACTION \n\t"
        "je "label-prefix"make_frac \n\t"
        ; rax is an integer
        "mov rdx, 1 \n\t"
        "DATA rax \n\t"
        "jmp "label-prefix"calculate \n\t"
        ""label-prefix"make_frac: \n\t"
        "CAR rax \n\t\t"
        "DATA rax\n"
        "CDR rdx \n\t"
        "DATA rdx\n"
        ""label-prefix"calculate:\n\t"
        "mov r12, rax\n"
        "mov r13, rdx\n"
        "mov r14, rbx\n"
        "mov r15, rcx\n"
        "MULTIPLY  r14, r15, r12, r13 \n\t"
        "mov rax, r12 \n"
        "mov rdx, r13 \n"
        "mov rbx, r10 \n"
        "mov rcx, r11 \n"

        "add r9, 1 \n\t"
        "jmp "label-prefix"loop \n\t"
        ""label-prefix"end_loop:\n\t"
        "REDUCER r10, r11 \n\t"

        "cmp r11, 1 \n\t"
        "jne "label-prefix"create_frac \n\t"
        ; Create integer.
        "ALLOCATE(8)\n"
        "mov rbx, r10 \n\t"
        "sal rbx, TYPE_BITS \n\t"
        "or rbx, T_INTEGER \n\t"
        "mov qword[rax], rbx\n"
        "jmp "label-prefix"create_end \n\t"

        ""label-prefix"create_frac:\n\t"
        ;Create fraction
        "ALLOCATE(8)\n"
        "sal r10, 4\n"
        "or r10, T_INTEGER\n"
        "mov qword[rax], r10\n"
        "mov r10, rax\n"
        "ALLOCATE(8)\n"
        "sal r11, 4\n"
        "or r11, T_INTEGER\n"
        "mov qword[rax], r11\n"
        "mov r11, rax\n"
        "ALLOCATE(8)\n"
        "MAKE_MALLOC_LITERAL_FRACTION rax, r10, r11\n"
        ""label-prefix"create_end: \n\t"

        ; rax holds result
        "leave\n\t"
        "ret\n\t"
      )
    )
  )
)

(define genBinBigger
  (lambda ()
    (let ((frac (getCount 'frac))(fracb (getCount 'fracb))(false (getCount 'false)))
      (string-append
        (start_assembly_library_functions 'bin-bigger)
        (getLabelFvar 'bin-bigger)"_body:\n"
        "push rbp\n\t"
        "mov rbp, rsp\n\t"
        "mov r12,qword[rbp+3*8] ;; number of args \n\t"
        "mov r10,qword[rbp+4*8] ;; r10 := first arg \n\t"
        "mov r11,qword[rbp+5*8] ;; r10 := Second arg \n\t"
        ;; get the value of r10 and r11
        "mov r10,[r10] ;; r10 :=the value first arg \n\t"
        "mov r11,[r11] ;; r11 :=the value Second arg \n\t"

        "mov rdx,r10 ;; check if arg is int or frac \n\t"
        "TYPE rdx \n\t"
        "cmp rdx,T_FRACTION \n\t"
        "je frac" frac "\n\t"
        "mov rdx,1 ;; assuming arg is int \n\t"
        "DATA r10 \n\t"
        "jmp after_check_frac" frac "\n\t"
        "frac" frac ": \n\t"
        "mov rdx,r10 \n\t"
        "DATA_UPPER r10 \n\t"
        "DATA_LOWER rdx \n\t"
        "after_check_frac" frac ":;r10/rdx \n\t"
        "mov rcx,rdx ;r10/rcx\n\t"

        "mov rdx,r11 ;; check if arg is int or frac \n\t"
        "TYPE rdx \n\t"
        "cmp rdx,T_FRACTION \n\t"
        "je fracb" fracb "\n\t"
        "mov rdx,1 ;; assuming arg is int \n\t"
        "DATA r11 \n\t"
        "jmp after_check_fracb" fracb "\n\t"
        "fracb" fracb ": \n\t"
        "mov rdx,r11 \n\t"
        "DATA_UPPER r11 \n\t"
        "DATA_LOWER rdx \n\t"
        "after_check_fracb" fracb ":;r11/rdx \n\t"

        ";multiply r10/rcx * r11/rdx to compare \n\t"
        "push rax \n\t"
        "mov rax,r10 \n\t"
        "mul rdx \n\t"
        "mov r10,rax \n\t"
        "mov rax,r11 \n\t"
        "mul rcx \n\t"
        "mov r11,rax \n\t"
        "pop rax \n\t"

        "cmp r10,r11 \n\t"
        "jle false" false "\n\t"
        "RET_TRUE ;; default answer is true \n\t"
        "false" false ":\n\t"
        "RET_FALSE \n\t"
      )
    )
  )
)
(define genBinEqual
	(lambda()
    (let ((frac (getCount 'frac))(fracb (getCount 'fracb))(true (getCount 'true)))
      (string-append
        (start_assembly_library_functions 'bin-equal)
        (getLabelFvar 'bin-equal)"_body:\n\t"
        "push rbp\n\t"
        "mov rbp, rsp\n\t"
        "mov r12,qword[rbp+3*8] ;; number of args \n\t"
        "mov r10,qword[rbp+4*8] ;; r10 := first arg \n\t"
        "mov r11,qword[rbp+5*8] ;; r11 := Second arg \n\t"
        ;; get the value of r10 and r11
        "mov r10,[r10] ;; r10 :=the value first arg \n\t"
        "mov r11,[r11] ;; r11 :=the value Second arg \n\t"
        "mov rdx,r10 ;; check if arg is int or frac \n\t"

        "TYPE rdx \n\t"
        "cmp rdx,T_FRACTION \n\t"
        "je frac" frac "\n\t"
        "mov rdx,1 ;; assuming arg is int \n\t"
        "DATA r10 \n\t"
        "jmp after_check_frac" frac "\n\t"
        "frac" frac ": \n\t"
        "mov rdx,r10 \n\t"
        "DATA_UPPER r10 \n\t"
        "DATA_LOWER rdx \n\t"
        "after_check_frac" frac ":;r10/rdx \n\t"
        "mov rcx,rdx ;r10/rcx\n\t"

        "mov rdx,r11 ;; check if arg is int or frac \n\t"
        "TYPE rdx \n\t"
        "cmp rdx,T_FRACTION \n\t"
        "je fracb" fracb "\n\t"
        "mov rdx,1 ;; assuming arg is int \n\t"
        "DATA r11 \n\t"
        "jmp after_check_fracb" fracb "\n\t"
        "fracb" fracb ": \n\t"
        "mov rdx,r11 \n\t"
        "DATA_UPPER r11 \n\t"
        "DATA_LOWER rdx \n\t"
        "after_check_fracb" fracb ":;r11/rdx \n\t"
        ;"mov rcx,rdx ;r11/rcx\n\t"

        ";multiply r10/rcx * r11/rdx to compare \n\t"
        "push rax \n\t"
        "mov rax,r10 \n\t"
        "mul rdx \n\t"
        "mov r10,rax \n\t"
        "mov rax,r11 \n\t"
        "mul rcx \n\t"
        "mov r11,rax \n\t"
        "pop rax \n\t"

        "cmp r10,r11 \n\t"
        "je true" true "\n\t"
        "RET_FALSE ;; default answer is true \n\t"
        "true" true ":\n\t"
        "RET_TRUE \n\t"

      )
    )
  )
)
(define checkTypeGen
  (lambda (tag type)
    (let ((curr (getCount "ret_true")))
      (string-append
        (start_assembly_library_functions tag)
        (getLabelFvar tag) "_body:\n\t"
        "push rbp\n\t"
        "mov rbp, rsp\n\t"
        ;; number of args == 1
        "mov r12, qword[rbp+3*8] \n\t"
        ;; r10 := first arg
        "mov r11, qword[rbp+4*8] \n\t"
        ;; get its value
        "mov r11,[r11] \n\t"
        ;;check the type
        "TYPE r11 \n \t\t"
        "cmp r11, " type "\n \t\t"
        "je return_true_" curr "\n\t"
        "RET_FALSE\n\t"
        "return_true_"curr ":\n\t\t"
        "RET_TRUE\n"
      )
    )
  )
)
(define genCons
  (lambda ()
    (string-append
      (start_assembly_library_functions 'cons)
      (getLabelFvar 'cons) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      "mov r12,qword[rbp+3*8] ;; number of args \n\t"
      "mov r10,qword[rbp+4*8] ;; r10 := car \n\t"
      "mov r11,qword[rbp+5*8] ;; r11 := cdr \n\t"
      ;; get the value of r10 and r11
      ;"mov r10,[r10] ;; r10 :=the value first arg \n\t"
      ;mov r11,[r11] ;; r11 :=the value Second arg \n\t"
      "ALLOCATE(8)\n\t"
      "MAKE_MALLOC_LITERAL_PAIR rax, r10, r11\n\t"
      "leave \n\t"
      "ret \n\t"
    )
  )
)
(define genCar
  (lambda ()
    (string-append
      (start_assembly_library_functions 'car)
      (getLabelFvar 'car) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      ;; r10 := first arg
      "mov r10,qword[rbp+4*8] \n\t"
      ;; get the value of r10 and r11
      "mov r10, [r10] ;; r10 := the value arg \n\t"
    	"DATA_UPPER r10\n"
    	"add r10, start_of_data\n"
      "mov rax, r10\n\t"
      "leave\n"
      "ret\n"

    )
  )
)
(define genCdr
  (lambda ()
    (string-append
      (start_assembly_library_functions 'cdr)
      (getLabelFvar 'cdr) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      ;; r10 := first arg
      "mov r10,qword[rbp+4*8] \n\t"
      ;; get the value of r10 and r11
      "mov r10,[r10] ;; r10 := the value arg \n\t"
    	"DATA_LOWER r10\n"
    	"add r10, start_of_data\n"
      "mov rax,r10\n\t"
      "leave\n\t"
      "ret\n"

    )
  )
)
(define genVectorRef
	(lambda()
		(string-append
      (start_assembly_library_functions 'vector-ref)
      (getLabelFvar 'vector-ref) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"

      "mov rax, An(0)\n\t"
      "mov rax, [rax]\n\t"
      "mov rbx, rax\n\t"

      "mov rcx, An(1)\n\t"
      "mov rcx, [rcx]\n\t"
      "mov rdx, rcx\n\t"
      "DATA rdx\n\t"

      "VECTOR_REF rcx, rbx, rdx\n\t"
      "push rcx\n\t"
      "ALLOCATE(8)\n\t"
      "pop rcx\n\t"
      "mov [rax], rcx\n\t"

      "leave\n\t"
      "ret\n"
    )
  )
)
(define genMakeString
  (lambda ()
    (string-append
      (start_assembly_library_functions 'make-string)
      (getLabelFvar 'make-string) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      "mov rdx, 0\n\t"

      "mov rbx, [rbp + 4*8]\n\t"
      "mov rbx,qword[rbx] \n\t"
      "DATA rbx \n\t"
      "ALLOCATE(rbx)\n\t"

      ;check number of arguments
      "mov rcx, qword [rbp + 3*8]\n\t"
      "cmp rcx, 2\n\t"
      "jne make_string_create\n\t"

      ";;get second argument to rdx\n\t"
      "mov rdx,qword [rbp + 5*8]\n\t"
      "mov rdx,qword [rdx]\n\t"
      "DATA rdx\n\t"
      "jmp make_string_create\n\t"


      ;rax= pointer to address of rbx*8 bytes,
      ;rbx=length of string
      "make_string_create:\n\t"
      "mov r10, 0\n\t" ;counter

      "make_string_regs_loop:\n\t\t"
      "cmp r10, rbx\n\t\t"
      "je end_of_create_string1\n\t\t"
      "mov byte [rax+r10], dl\n\t\t"
      "inc r10\n\t\t"
      "jmp make_string_regs_loop\n\t"
      "end_of_create_string1:\n\t\t"

      "mov rcx, rax\n\t\t"
      ;"shl rbx, 3\n\t\t"
      "MAKE_LITERAL_STRING_REGS rcx, rbx\n\t\t" ;rax = literal vector
      "mov rcx, rax\n\t\t"
      "ALLOCATE(8)\n\t\t"
      "mov [rax], rcx\n\t\t"

      "leave\n\t\t"
      "ret\n\t\t"
    )
  )
)
(define genMakeVector
  (lambda ()
    (string-append
      (start_assembly_library_functions 'make-vector)
      (getLabelFvar 'make-vector) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"

      "mov rbx, [rbp + 4*8]\n\t"
      "mov rbx,qword[rbx] \n\t"
      "DATA rbx \n\t"
      "mov rdi,rbx\n\t"
      "sal rdi, 3\n\t"
      "ALLOCATE(rdi)\n\t"

      ;check number of arguments
      "mov rcx, qword [rbp + 3*8]\n\t"
      "cmp rcx, 2\n\t"
      "jne make_vector_with_one_arg\n\t"

      ";;get second argument to rdx\n\t"
      "mov rdx,qword [rbp + 5*8]\n\t"
      "jmp make_vector_create_vector\n\t"

      "make_vector_with_one_arg:\n\t\t"
      "push rax\n\t\t"
      "mov rdx,0\n\t\t"
      "mov rcx, rdx \n\t\t"
      "sal rcx, TYPE_BITS \n\t\t"
      "or rcx, T_INTEGER \n\t\t"
      "ALLOCATE(8)\n\t\t"
      "mov qword[rax], rcx\n\t\t"
      "mov rdx,rax\n\t\t"
      "pop rax\n\t\t"

      ;rax= pointer to address of rbx*8 bytes,
      ;rbx=length of vector
      "make_vector_create_vector:\n\t"
      "mov r10, 0\n\t" ;counter

      "make_vector_regs_loop:\n\t\t"
      "cmp r10, rbx\n\t\t"
      "je end_of_vector1\n\t\t"

      "mov qword [rax+r10*8], rdx\n\t\t"
      "inc r10\n\t\t"
      "jmp make_vector_regs_loop\n\t"
      "end_of_vector1:\n\t\t"

      "mov rcx, rax\n\t\t"
      "sal rbx, 3\n\t\t"
      "MAKE_LITERAL_VECTOR_REGS rcx, rbx\n\t\t" ;rax = literal vector
      "mov rcx, rax\n\t\t"
      "ALLOCATE(8)\n\t\t"
      "mov [rax], rcx\n\t\t"

      "leave\n\t\t"
      "ret\n\t\t"
    )
  )
)
(define genVector
  (lambda ()
    (string-append
      (start_assembly_library_functions 'vector)
      (getLabelFvar 'vector) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      "mov rbx, arg_count\n\t"

      "mov rdi, rbx\n\t"
      "sal rdi, 3\n\t"
      "ALLOCATE(rdi)\n\t"

      ;rax= pointer to address of rbx*8 bytes,
      ;rbx=length of vector
      "mov r10, 0\n\t" ;counter
      "vector_regs_loop:\n\t\t"
      "cmp r10, rbx\n\t\t"
      "je end_of_vector\n\t\t"

      "mov rdx, An(r10)\n\t\t"
      "mov qword [rax+r10*8], rdx\n\t\t"
      "inc r10\n\t\t"
      "jmp vector_regs_loop\n\t"
      "end_of_vector:\n\t\t"

      "mov rcx, rax\n\t\t"
      "sal rbx, 3\n\t\t"
      "MAKE_LITERAL_VECTOR_REGS rcx, rbx\n\t\t" ;rax = literal vector
      "mov rcx, rax\n\t\t"
      "ALLOCATE(8)\n\t\t"
      "mov [rax], rcx\n\t\t"

      "leave\n\t\t"
      "ret\n\t\t"
    )
  )
)
(define genVectorLength
  (lambda ()
    (string-append
      (start_assembly_library_functions 'vector-length)
      (getLabelFvar 'vector-length) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      ;; r10 := first arg
      "mov r10,qword[rbp+4*8] \n\t"
      ;; get the value of r10 and r11
      "mov r10,[r10] ;; r10 := first arg \n\t"
      "VECTOR_LENGTH r10\n\t"
      "ALLOCATE(8)\n"
      "mov rbx, r10 \n\t"
      "sal rbx, TYPE_BITS \n\t"
      "or rbx, T_INTEGER \n\t"
      "mov qword[rax], rbx\n"
      "leave\n"
      "ret\n"
    )
  )
)
(define genStringLength
  (lambda ()
    (string-append
      (start_assembly_library_functions 'string-length)
      (getLabelFvar 'string-length) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      ;; r10 := first arg
      "mov r10,qword[rbp+4*8] \n\t"
      ;; get the value of r10 and r11
      "mov r10,[r10] ;; r10 := first arg \n\t"
      "STRING_LENGTH r10\n\t"
      "ALLOCATE(8)\n"
      "mov rbx, r10 \n\t"
      "sal rbx, TYPE_BITS \n\t"
      "or rbx, T_INTEGER \n\t"
      "mov qword[rax], rbx\n"
      "leave\n"
      "ret\n"
    )
  )
)
(define genIntegerToChar
  (lambda ()
    (string-append
      (start_assembly_library_functions 'integer->char)
      (getLabelFvar 'integer->char) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      "mov r12,qword[rbp+3*8] ;; number of args \n\t"

      ;"cmp r12, 1\n\t"
      ;"jne E_bad_arg_count\n\t"
      ;; rdx := first arg
      "mov rdx,qword[rbp+4*8] \n\t"
      ;; get the value of rdx
      "mov rdx,[rdx] ;; rdx := the value first arg \n\t"
      ;"mov rbx, rdx \n\t"
      ;"TYPE rbx \n\t"
      ;"cmp rbx, T_INTEGER\n\t"
      ;"jne E_bad_type"
      ;"mov rbx, rdx\n\t"
      ;"DATA rbx\n\t"
      ;"cmp rbx, 0"
      ;"jl .E_bad_value"
      ;"cmp rbx, 256"
      ;"jge .E_bad_value"
      "xor rdx,(T_CHAR^T_INTEGER)\n\t"
      "ALLOCATE(8)\n\t"
      ;"sal rdx, 4\nor rdx, T_INTEGER\n push rdx\n call write_sob_if_not_void\n"
      "mov qword[rax], rdx\n"
      "leave\n\t"
      "ret\n\t"
    )
  )
)
(define genCharToInteger
  (lambda ()
    (string-append
      (start_assembly_library_functions 'char->integer)
      (getLabelFvar 'char->integer) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      "mov r12,qword[rbp+3*8] ;; number of args \n\t"


      "mov rdx,qword[rbp+4*8] \n\t"
      "mov rdx,[rdx] ;; rdx := the value first arg \n\t"

      "xor rdx,(T_CHAR^T_INTEGER)\n\t"
      "ALLOCATE(8)\n\t"
      ;"sal rdx, 4\nor rdx, T_INTEGER\n push rdx\n call write_sob_if_not_void\n"
      "mov qword[rax], rdx\n"
      "leave\n\t"
      "ret\n\t"
    )
  )
)
(define start_assembly_library_functions
  (lambda (tag)
    (string-append
      "start_"(getLabelFvar tag)":\n\t"
      "ALLOCATE(16)\n\t"
      "MAKE_LITERAL_CLOSURE rax, 0, " (getLabelFvar tag) "_body\n\t"
      "mov rax, [rax]\n\t"
	    "mov qword[" (getLabelFvar tag) "], rax\n\t"
      "ret\n"
    )
  )
)
(define start_scheme_library_functions
  (lambda (tag code)
    (string-append
      "start_"(getLabelFvar tag)":\n\t"
      (code-gen
        (car
          (pipeline
            (string->list code)
          )
        )
      )
      "ret\n"
    )
  )
)
