  ;; Created by Amir Arbel and Ronen Finish
;; @file genLibraryFunctions.scm
;; Contains code generation functions for:
; apply


(define genApply
  (lambda ()
    (string-append
      (start_assembly_library_functions 'apply)
      (getLabelFvar 'apply)"_body:\n\t"
       "push rbp\n\t"
       "mov rbp, rsp\n\t"
       "mov r10, qword [rbp]\n\t"
       "mov r11, qword [rbp+8]\n\t"
       ; First arg. rax - closure
       "mov rax, An(0)\n\t"
       "mov rax, [rax]\n\t"
       ; r12 - end of current args
       "mov r12, rbp\n\t"
       "add r12, 6*8\n\t"

      "mov r9, rax\n\t"
      "TYPE r9\n\t"
      "cmp r9, T_CLOSURE\n\t"
      "jne L_die_not_closure\n\t"
      ;; rcx the list of variables.
      "mov rcx, [rbp + 8*5]  \n\t"
      "mov rcx, [rcx]\n\t"
      ; Count the variables.
      "mov rbx, rcx\n\t"
      "mov rsi, 0\n\t"
      "L_apply_count_loop:\n\t"
      "cmp rbx, T_NIL\n\t\t"
      "je L_apply_count_loop_end\n\t"
      "CDR rcx\n\t"
      "mov rbx, rcx\n\t"
      "TYPE rbx\n\t"
      "add rsi, 1\n\t"
      "jmp L_apply_count_loop\n"
      "L_apply_count_loop_end:\n\t"

      "sal rsi, 3\n"
      "sub r12, rsi\n"
      "sar rsi, 3\n\t"
      "mov rdi, 0\n\t"
      "mov rcx, [rbp + 8*5]\n\t"
      "mov rcx, [rcx]\n\t"

      "L_apply_push_args_loop:\n"
      "cmp rdi, rsi\n\t"
      "je L_apply_push_args_loop_exit\n\t"
      "mov rbx, rcx\n\t"
      "DATA_UPPER rbx\n\t"
      "add rbx, start_of_data\n\t"
      "mov qword [r12 + 8*rdi], rbx\n\t"
      "CDR rcx\n\t"
      "add rdi, 1\n\t"
      "jmp L_apply_push_args_loop\n"
      "L_apply_push_args_loop_exit:\n"
      "push sobNil\n\t"
      ; Update number of variables.
      "sub r12, 8\n\t"
      "mov qword [r12], rsi\n\t"

      ; Update the closure environment
      "sub r12, 8\n\t"
      "mov rbx, rax\n\t"
      "CLOSURE_ENV rbx\n\t"
      "mov qword [r12], rbx\n\t"

      "sub r12, 8\n\t"
      "mov qword [r12], r11\n\t"
      "mov rsp, r12\n\t"
      "mov rbp, r10\n\t"
      "mov rbx, rax\n\t"
      ; Without this, we might jump to something not closure-y
      "TYPE rbx\n\t"
      "cmp rbx, T_CLOSURE\n\t"
      "jne apply_exit_error\n\t"
      "CLOSURE_CODE rax\n\t"
      "jmp rax\n\t"

      "apply_exit_error:\n"
      "leave\n"
      "ret\n"
    )
  )
)
(define genSymbol->string
  (lambda ()
    (string-append
      (start_assembly_library_functions 'symbol->string)
      (getLabelFvar 'symbol->string) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      "mov rax, An(0)\n"
      "mov rax, [rax]\n"
      "DATA rax\n"
      "add rax, start_of_data\n"
      "leave\n\t"
      "ret\n\t"
    )
  )
)
(define genString-ref
  (lambda ()
    (string-append
      (start_assembly_library_functions 'string-ref)
      (getLabelFvar 'string-ref) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      "mov rbx, An(0)\n"
      "mov rcx, An(1)\n"
      "mov rcx, [rcx]\n"
      "DATA rcx\n"
      "mov rbx, [rbx]\n"
      "STRING_REF rax, rbx, rcx\n"
      "shl rax, 4\n"
      "or rax, T_CHAR\n"
      "mov rbx, rax\n"
      "ALLOCATE(8)\n"
      "mov qword[rax], rbx\n"
      "leave\n\t"
      "ret\n\t"
    )
  )
)

(define genNumerator
  (lambda ()
    (string-append
      (start_assembly_library_functions 'numerator)
      (getLabelFvar 'numerator) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      "mov rax, An(0)\n"
      "mov rax, [rax]\n"
      "DATA_UPPER rax\n"
      "add rax, start_of_data\n"
      "leave\n\t"
      "ret\n\t"
    )
  )
)
(define genDenominator
  (lambda ()
    (string-append
      (start_assembly_library_functions 'denominator)
      (getLabelFvar 'denominator) "_body:\n\t"
      "push rbp\n\t"
      "mov rbp, rsp\n\t"
      "mov rax, An(0)\n"
      "mov rax, [rax]\n"
      "mov rbx, rax\n"
      "TYPE rbx\n"
      "cmp rbx, T_INTEGER\n"
      "je denominator_integer\n"
      "DATA_LOWER rax\n"
      "add rax, start_of_data\n"
      "jmp denominator_exit\n"
      "denominator_integer: \n"
      "mov rbx, 1\n"
      "shl rbx, 4\n"
      "or rbx, T_INTEGER\n"
      "ALLOCATE(8)\n"
      "mov qword[rax], rbx\n"
      "denominator_exit:"
      "leave\n\t"
      "ret\n\t"
    )
  )
)


(define genSetString
  (lambda ()
  (string-append
    (start_assembly_library_functions 'string-set!)
    (getLabelFvar 'string-set!) "_body:\n\t"
    "push rbp\n\t"
    "mov rbp, rsp\n\t"
    "mov r11, An(0) \n" ; string
    "mov r11, [r11]\n"
    "mov r12, An(1)\n" ;  index
    "mov r12, [r12]\n"
    "mov rax, An(2) \n" ; char
    "mov rax, [rax]\n"
    "DATA rax \n"
    "STRING_ELEMENTS r11 \n"
    "DATA r12 \n"
    "inc r12 \n"
    "mov rcx , 0 \n"
    "string_set_loop: \n
    cmp rcx, r12 \n
    je done_string_set \n
    mov bl, byte [r11] \n
    and rbx, 0xff \n"
    "inc rcx \n"
    "inc r11 \n"
    "jmp string_set_loop \n"


    "done_string_set: \n"
    "mov byte [r11-1], al  \n" ; set! char in string

    "mov rax, sobVoid \n"
    "leave\n"
    "ret\n"
    )
  )
)
