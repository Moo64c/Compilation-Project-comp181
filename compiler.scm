(define compile-scheme-file
  (lambda (scmFile assFile)
    (let*
      (
        (out-port (open-output-file assFile 'truncate))
        (parsedFile (pipeline (append globalLibraryWeirdNamesFixes (file->list scmFile))))
        (ConstTableStr (generateConstTable parsedFile))
        (globalTable (generateGlobalTable parsedFile))
        (write_to_file (lambda (s) (display s out-port)))
      )

      (write_to_file
        (gen-all-code

          (genLibraryFunctions)
          (getConstTableInitialiStr)
          (getGlobalTableInitialiStr)
          (getConstTableAssignmentsStr)
          (getGlobalTableAssignmentsStr)
          ; Push all generated parameter expressions to the stack.
          (fold-right
            string-append
            ""
            (cMap
              (lambda (x)
                (string-append
                  (code-gen x)
                  ; Print result.
                  "mov rax, [rax]\n"
                  "push rax \n"
                  "call write_sob_if_not_void \n"
                  "add rsp, 1*8\n"
                )
              )
              parsedFile
            )
          )
        )
       ;(display ConstTableStr)
       ;(display "\n==========codegen\n")
        ;[....]
      )
      (write_to_file (string-append
        ";"
        ;(toString parsedFile)
        "\n"
        )
      )
    )
  )
)


(define run-parser
  (lambda (parser lst)
    (parser lst
	    (lambda (e s)
	      '(,e ,(run-parser parser s)))
    )
	)
)
;make recursive, should return list of spexr;
(define pipeline
  (lambda (s)
    ((star <sexpr>) s
    (lambda (m r)
      (map
        (lambda (e)
          (annotate-tc
            (pe->lex-pe
              (box-set
                (remove-applic-lambda-nil
                  (parse
                    e
                  )
                )
              )
            )
          )
        )
        m))
      (lambda (f) 'fail))))

(define file->list
  (lambda (in-file)
    (let
      ((in-port (open-input-file in-file)))
      (letrec ((run
        (lambda ()
          (let ((ch (read-char in-port)))
            (if (eof-object? ch)
              (begin
              (close-input-port in-port)
              '())
                (cons ch (run)))))))
                  (run)
      )
    )
  )
)
