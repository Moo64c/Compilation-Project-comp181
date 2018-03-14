;; Created by Amir Arbel and Ronen Finish
; @file assembly_lambdas
; Holds lambda-simple, var, and opt code.

(define major -1 )

;; (lambda-simple (var1 var2...) e)
(define translate-lambda-simple
  (lambda (expression)
    ; Increment major counter
    (set! major (+ major 1))
    (let*
      (
        (lambda_simple_index (getCount 'lambdasimple))
        (code
          (string-append
            "ALLOCATE(8 * " (number->string (+ 1 major)) ")\n"
            "mov rbx, rax\n"
            ; Number of lambda nesting
            "mov r11, 0\n"
            "cmp r11, " (number->string major) "\n"
            "je L_lambda_not_nested_" lambda_simple_index "\n"
            ; Number of variables from last lambda.
            "mov r11, qword [rbp + (3 * 8)] \n"
            "mov r9, r11 \n"
            "sal r9, 3 \n"  ; r9*=8
            "ALLOCATE(r9) \n"
            ; number of vars in last lambda.
            "mov rcx, rax \n"
            ; Index register - r10 = 0
            "mov r10, 0\n"
            "L_lambda_params_loop_" lambda_simple_index ":\n"
            ; Check indexes.
            "cmp r10, r11 \n"
            "je L_lambda_end_params_loop_" lambda_simple_index "\n"
            ;; with the following line
            "mov r12, qword [rbp + ((4 + r10) * 8)]\n"
             ;; rcx[i]:=par[i]
            "mov qword [rcx + (r10 * 8)], r12 \n"
            ;; Increment r10
            "add r10, 1\n"
            "jmp L_lambda_params_loop_" lambda_simple_index "\n"
            "L_lambda_end_params_loop_" lambda_simple_index ":\n"

            ; Save pointers to environments.
            ; Copy environment to r12.
            "mov r12, qword [rbp + (2 * 8)] \n"
            ; Stack index r10, environment index r11
            "mov r10, 0\n"
            "mov r11, 1\n"
            "L_lambda_env_loop_" lambda_simple_index ":\n"
            "cmp r10," (number->string major) "\n"
            "je L_lambda_end_env_loop_" lambda_simple_index "\n"
            ;; with following line
            "mov r12, qword [r12+(r10 * 8)]\n"
            ;; rbx[stack index] <= env[env index]
            "mov qword [rbx + r11], r12\n"
            ;; Increment both indexes.
            "add r10, 1\n"
            "add r11, 1\n"
            "jmp L_lambda_env_loop_" lambda_simple_index "\n"
            "L_lambda_end_env_loop_" lambda_simple_index ":\n"
            ;; rbx[0] <= rcx
            "mov qword [rbx], rcx\n"
            "L_lambda_not_nested_" lambda_simple_index ":\n"

            ; push to the stack the trio - T_CLOSURE, env, code
            ; For some reason not clear to me it's 16 bytes.
            "ALLOCATE(16)\n"
            "jmp L_lambda_simple_closure_exit_" lambda_simple_index "\n"

            "L_lambda_simple_closure_body_" lambda_simple_index ":\n"
            "\t push rbp \n"
            "\t mov rbp, rsp \n"
            (fold-right
              string-append
              ""
              (cMap code-gen (cddr expression))
            )
            "\t leave \n "
            "\t ret \n "
            "L_lambda_simple_closure_exit_" lambda_simple_index ": \n"
            "MAKE_LITERAL_CLOSURE rax, rbx, L_lambda_simple_closure_body_" lambda_simple_index "\n\t"
          )
        )
      )
      (set! major (- major 1))
      code
    )
  )
)

(define translate-lambda-opt
  (lambda (parsed-expression)
    (set! major (+ major 1))
    (let*
      (
        (lambda-opt-count (getCount 'lambdaopt))
        ; Number of variables to store normally.
        (number-of-normal-vars (length (cadr parsed-expression)))
        (code (string-append
            ; Save rax.
            "push rax\n"
            "ALLOCATE(8 * " (number->string (+ 1 major)) ")\n"
            "mov rbx, rax\n"
            "pop rax\n"
            ; Number of lambda nesting
            "mov r11, 0\n"
            "cmp r11, " (number->string major) "\n"
            "je L_lambda_opt_not_nested_" lambda-opt-count "\n"
            ; Number of variables from last lambda.
            "mov r11, qword[rbp + (3 * 8)] \n"
            "mov r9, r11 \n"
            "sal r9, 3 \n"  ; r9*=8
            "ALLOCATE(r9) \n"
            ; number of vars in last lambda.
            "mov rcx, rax \n"
            ; Index register - r10 = 0
            "mov r10, 0\n"
            "L_lambda_opt_params_loop_" lambda-opt-count ":\n"
            ; Check indexes.
            "cmp r10, r11 \n"
            "je L_lambda_opt_end_params_loop_" lambda-opt-count "\n"
            ;; with the following line
            "mov r12, qword[rbp + ((4 + r10) * 8)]\n"
             ;; rcx[i]:=par[i]
            "mov qword[rcx + (r10 * 8)], r12 \n"
            ;; Increment r10
            "add r10, 1\n"
            "jmp L_lambda_opt_params_loop_" lambda-opt-count "\n"
            "L_lambda_opt_end_params_loop_" lambda-opt-count ":\n"

            ; Save pointers to environments.
            ; Copy environment to r12.
            "mov r12, qword[rbp + (2 * 8)] \n"
            ; Stack index r10, environment index r11
            "mov r10, 0\n"
            "mov r11, 1\n"
            "L_lambda_opt_env_loop_" lambda-opt-count ":\n"
            "cmp r10," (number->string major) "\n"
            "je L_lambda_opt_end_env_loop_" lambda-opt-count "\n"
            ;; with following line
            "mov r12, qword[r12+(r10 * 8)]\n"
            ;; rbx[stack index] <= env[env index]
            "mov qword[rbx + r11], r12\n"
            ;; Increment both indexes.
            "add r10, 1\n"
            "add r11, 1\n"
            "jmp L_lambda_opt_env_loop_" lambda-opt-count "\n"
            "L_lambda_opt_end_env_loop_" lambda-opt-count ":\n"
            ;; rbx[0] <= rcx
            "mov qword[rbx], rcx\n"
            "L_lambda_opt_not_nested_" lambda-opt-count ":\n"

            ; push to the stack the trio - T_CLOSURE, env, code
            "ALLOCATE(8)\n"
            "jmp L_lambda_opt_closure_exit_" lambda-opt-count "\n"

            "L_lambda_opt_closure_body_" lambda-opt-count ":\n"
            "\t push rbp \n"
            "\t mov rbp, rsp \n"

            ;; Fix lambda-opt argument list.
            ; Start the list.
            ; r13 will hold the list itself.
            "mov r13, sobNil\n"
            ; r14 is loop counter. Starts at (number of variables - 1).
            ; We got x variables in the lambda body, we want to skip the
            ; normal ones and create a list at the end.
            ; Total number of variables.
            "mov r14, qword[rbp+24]\n"
            "dec r14\n"
            ; r15 - amount of variables.
            "mov r15, "(number->string (- number-of-normal-vars 1)) "\n"
            "L_clos_body_fix_optional_" lambda-opt-count ":\n"
            "cmp r14, r15 \n"
            "je L_clos_body_fix_optional_end_" lambda-opt-count "\n"
            ; Find the last variable in the list
            "mov r8, qword[rbp+(r14+4)*8]\n"
            ; This is a pointer... to a pointer.
            ; Reduce one level of pointers.
            ; Create the space for the new pair.
            "ALLOCATE(8)\n"
            "MAKE_MALLOC_LITERAL_PAIR rax, r8, r13\n"
            ; Update the current newest pair.
            "mov r13, rax\n"
            ; Decrease by 1 number of loops to go.
            "dec r14\n"
            "jmp L_clos_body_fix_optional_" lambda-opt-count ";\n"
            "L_clos_body_fix_optional_end_" lambda-opt-count ":\n"
            ; We need to add the list at the end of the normal variables.
            "mov qword [rbp + (r15+1+4)*8], r13\n"

            (fold-right
              string-append
              ""
              (cMap code-gen (cdddr parsed-expression))
            )
            "\tleave\n"
            "\tret;\n"
            "L_clos_optional_exit_" lambda-opt-count ":\n"
            "L_lambda_opt_closure_exit_" lambda-opt-count ": \n"
            "MAKE_LITERAL_CLOSURE rax, rbx, L_lambda_opt_closure_body_" lambda-opt-count "\n\t"
          )
        )
      )
      (set! major (- major 1))
      code
    )
  )
)
