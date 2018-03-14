;; Created by Amir Arbel and Ronen Finish
; @file assembly_box
; Holds box code

(define translate-set
  (lambda (set-exp)
    (let
      (
        (var (cadr set-exp))
      )
      (string-append
        (code-gen (caddr set-exp))
        (cond
          ((equal? (car var) 'bvar)
            (string-append
            "mov rbx, qword[rbp+2*8] \n"
            "mov rbx, qword[rbx+"
            (number->string (caddr var))
            "*8] \n"
            "mov qword[rbx+"
            (number->string (cadddr var))
            "*8], rax \n"
            "mov rax, sobVoid \n")
          )
          ((equal? (car var) 'pvar)
            (string-append
              "mov qword[rbp+(4+"
              (number->string (caddr var))
              ")*8], rax \n"
              "mov rax, sobVoid \n"))
          ((equal? (car var) 'fvar)
            (string-append
              (code-gen (caddr set-exp))
              "\n mov rbx, rax \n"
              "mov rax, "
              (toString
                (getLabelFvar
                  (cadr parsed-expression)
                )
              ) "\n"
            ;change the value old rax
            "mov qword[rax], rbx \n"
            "mov rax, sobVoid \n"
            )
          )
        )
      )
    )
  )
)

(define translate-box
  (lambda (parsed-expression)
    (string-append
      (code-gen (cadr parsed-expression))
      ;(box pe)
      ";allocate space for box"
      "mov rbx, rax\n\t"
      "ALLOCATE 8\n\t"
      "mov qword [rax], rbx\n\t"
    )
  )
)

(define translate-box-get
  (lambda (parsed-expression)
    (let
      (
        (tag (caadr parsed-expression))
      )
      (cond
        ((equal? tag 'pvar)
          (let
            (
              (minor (car (cddadr parsed-expression)))
            )
            (string-append
              "mov rbx, 1\n\t"
              "add rbx, " (toString minor)"\n\t"
              "mov rax, An(rbx)\n"
            )
          )
        )
        ((equal? tag 'bvar)
          (let
            (
              (major (car (cddadr parsed-expression)))
              (minor (car (cdr (cddadr parsed-expression))))
            )
            (string-append
              "mov rbx, An(0)\n\t"
              "add rbx, "(toString major) "\n\t"
              "mov rbx, An(rbx)\n\t"
              "add rbx, " (toString minor) "\n\t"
              "mov rax, qword[rbx]\n\t"
            )
          )
        )
        ((equal? tag 'fvar)
          (string-append
            "mov rax, qword[" (toString (getLabelFvar (cadr parsed-expression))) "]\n"
          )
        )
        (else "BOX GET ERROR")
      )
    )
  )
)

(define translate-box-set
  (lambda (pe)
    (let
      (
        (tag (caadr pe))
        (var (cadadr pe))
        (val (code-gen (caddr pe)))
      )
      (cond
        ((equal? tag 'pvar)
          (let
            (
              (minor (car (cddadr pe)))
            )
            (string-append
              val
              "mov rbx, 2\n"
              "add rbx, " (toString minor) "\n"
              "mov An(rbx), rax\n"
              "mov rax, sobVoid\n")
          )
        )
        ((equal? tag 'bvar)
          (let ((major (car (cddadr pe)))
            (minor (car(cdr (cddadr pe)))))
            (string-append
            val
            "MOV(R1, FPARG(0));\n"
            "MOV(R1, INDD(R1,"(toString major)"));\n"
            "MOV(R1, INDD(R1,"(toString minor)"));\n"
            "MOV(IND(R1), R0);\n"
            "MOV(R0, IMM(SOB_VOID));\n"
            )))
            ((equal? tag 'fvar)
            (string-append
            val
            "MOV(R2, R0);\n"
            "PUSH(1);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(IND(R0), R2);\n"
            "MOV(IND(" (toString (getMemFromTable var fvarTable)) "), R0);\n"
            "MOV(R0, IMM(SOB_VOID));\n"
            ))
            (else "WRONG"))))
)
