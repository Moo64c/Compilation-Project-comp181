;; Created by Amir Arbel and Ronen Finish
; @file assembly_variables
; Holds const, fvar, pvar, bvar codes.
; @ Depedencies ./utility.scm, constantTable.scm


; (const [constant_name])
; MOV RAX, [const-label]
(define translate-const
  (lambda (parsed-expression)
    (string-append
      "mov rax, "
      (toString
        (getLabelFromTable
          (getIndexFromTable
            (cadr parsed-expression)
            constantTable
          )
        )
      )
      "\n"
    )
  )
)

;;(define (fvar name) expression)
;; mov qword[label-of-fvar], [[expression]]
;; mov rax, #void
(define translate-define
  (lambda (parsed-expression)
    (string-append
      ; Execute expression.
      (code-gen (caddr parsed-expression))
      ; Save to label position.
      "mov rax, [rax]\n"
      "mov qword["
      (toString
        (getLabelFvar
          (cadadr parsed-expression)
        )
      )
      "], rax \n"
      ; Return void.
      "mov rax, sobVoid\n"
    )
  )
)

;;(fvar name)
;; mov rax, qword[label-of-fvar]
(define translate-fvar
  (lambda (parsed-expression)
    (string-append
      "mov rax, "
      (toString
        (getLabelFvar
          (cadr parsed-expression)
        )
      )
      "\n"
    )
  )
)

;;(pvar name minor)
(define translate-pvar
  (lambda (parsed-expression)
    (string-append
      "; pvar " (symbol->string (cadr parsed-expression)) "\n"
      "mov rax, qword[rbp+8*(4+"
      (number->string (caddr parsed-expression))
      ")]\n"
    )
  )
)

;;(bvar name major minor)
(define translate-bvar
  (lambda (parsed-expression)
    (string-append
      "; Environment\n"
      "mov rax, qword [rbp + 2*8]\n"
      "; Environment[major]\n"
      "mov rax, qword [rax + 8*"
      (number->string (caddr parsed-expression))
      "]\n"
      "; Environment[major][minor]\n"
      "mov rax, qword[rax + 8*"
      (number->string (cadddr parsed-expression))
      "]\n"
    )
  )
)
