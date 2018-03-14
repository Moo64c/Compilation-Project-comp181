;; Created by Amir Arbel and Ronen Finish
; @file assembly_conditionals
; Holds if and or code.

(define get-if-count
  (lambda () (getCount 'if))
)


(define translate-if3
  (lambda (parsed-expression)
    (let
      (
        ; the current if statement index (in global scope)
        (if-index (get-if-count))
      )
      (string-append
          (code-gen (cadr parsed-expression)) "\n"
          "mov rax, qword[rax]\n"
          "CMP RAX, SOB_FALSE \n"
          "JE Lelse_then_" if-index "\n"
          (code-gen (caddr parsed-expression)) "\n"
          "JMP Lend_if_" if-index "\n"
          "Lelse_then_" if-index ":\n"
          (code-gen (cadddr parsed-expression)) "\n"
          "Lend_if_" if-index ":\n"
      )
    )
  )
)


(define translate-or
  (lambda (expressions)
    (letrec
      (
        ; the current if statement index (in global scope)
        (end-or (string-append "end_or_" (get-if-count)))
      )
      (string-append
        (fold-right
          string-append
          ""
          (cMap
            (lambda (condition)
              (string-append
                (code-gen condition) "\n"
                "cmp rax, sobFalse\n"
                "jne " end-or "\n "
              )
            )
            (car (cdr expressions))
          )
        )
        end-or ":\n"
      )
    )
  )
)
