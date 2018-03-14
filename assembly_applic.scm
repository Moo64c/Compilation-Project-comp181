;; Created by Amir Arbel and Ronen Finish
; @file assembly_applic
; Holds applic and tc_applic code

; Pass in list w/o applic (applic proc e1 e2 e3) => (proc e1 e2 3)
(define translate-applic
  ; Act according to proc's type.
  (lambda (expression)
    (let
      (
        (reversed (reverse (caddr expression)))
      )
      (string-append
        ; Push all generated parameter expressions to the stack.
        (fold-right
          string-append
          ""
          (map
            (lambda (parameter_expression)
              (string-append
                ";generate expression and push parameter\n"
                (code-gen parameter_expression)
                "push rax\n\t"
              )
            )
            reversed
          )
        )
        ; Push number of variables.
        ";number of variables\n"
        "push " (number->string (length reversed)) " \n"
        ; Check proc part of expression
        (code-gen (cadr expression))
        ; If type != closure
        "mov rbx, qword [rax] \n"
        "TYPE rbx \n"
        "cmp rbx, T_CLOSURE \n"
        "jne L_die_not_closure \n"
        ; Get environment, push to stack.
        "mov rbx, [rax]\n"
        "CLOSURE_ENV rbx\n"
        "push rbx \n"
        ; Call function.
        "mov rbx, [rax]\n"
        "CLOSURE_CODE rbx\n"
        "call rbx \n"
        ; Pop number of arguments
        "add rsp, 8*(" (number->string (+ 2 (length reversed))) ")\n" ;env
      )
    )
  )
)

(define translate-tc-applic
  ; Act according to proc's type.
  (lambda (expression)
    (let
      (
        (reversed (reverse (caddr expression)))
        (applic-index (getCount 'applic))
      )
      (string-append
        ; Push all generated parameter expressions to the stack.
        (fold-right
          string-append
          ";Push parameters\n\t"
          (map
            (lambda (parameter_expression)
              (string-append
                (code-gen parameter_expression)
                "push rax\n\t"
              )
            )
            reversed
          )
        )
        ; Push number of variables.
        "push " (number->string (+ (length reversed) 1)) " \n\t"
        ; Check proc part of expression
        (code-gen (cadr expression))
        ; If type != closure
        "mov rbx, qword [rax] \n\t"
        "TYPE rbx \n\t"
        "cmp rbx, T_CLOSURE \n\t"
        "jne L_die_not_closure \n\t"

        "mov rax, [rax]\n\t"
        "mov rbx, rax\n\t"
        "CLOSURE_ENV rbx\n\t"
        "push rbx\n\t"
        ; Override return value.
        "mov r8, qword [rbp + 8]\n\t"
        "push r8 \n\t"
        ; r8 - original rbp [fp].
        "mov r8, rbp \n\t"
        ; Get old rbp [fp] value.
        "mov rbp, qword [rbp]\n\t"
        ;; Initialize loop variables.
        ; r13 - parameter index - number of parameters in old rbp + 4
        "mov r13, qword [r8 + 24]\n\t"
        "add r13, 4\n\t"
        ; r11 -
        "mov r11, " (number->string (+ 3 (length reversed))) "\n"
        ; Loop counter.
        "mov r10, 1\n"

        "L_tc_applic_loop_" applic-index ":\n\t"
        "cmp r10, r11 \n\t"
        "jg L_end_tc_applic_loop_" applic-index " \n\t"
        "sub r13, r10 \n\t"
        "sub r11, r10 \n\t"
        ; Copy parameter from source.
        "mov r9, qword [rsp + ((r11) * 8)] \n\t"
        ; To destination (old rbp + 8*parameter index).
        "mov qword [r8+8*(r13)], r9\n\t" ;; r9 -> dest
        "add r13, r10 \n\t"
        "add r11, r10 \n\t"
        "add r10, 1 \n\t"
        "jmp L_tc_applic_loop_" applic-index " \n"
        "L_end_tc_applic_loop_" applic-index ": \n\t"
        ; rsp := old rbp.
        "mov rsp, r8 \n\t"
        "sal r13, 3 \n\t"
        "add rsp, r13 \n\t"
        ; Reduce stack pointer by r11 * 8
        "sal r11, 3 \n\t"
        "sub rsp, r11 \n\t"
        "CLOSURE_CODE rax \n\t"
        "jmp rax \n\t"
      )
    )
  )
)

;; (seq e1 e2 ...)
(define translate-sequence
  (lambda (parsed-expression)
    (fold-right
      string-append
      ""
      (map
        code-gen
        (cadr parsed-expression)
      )
    )
  )
)
