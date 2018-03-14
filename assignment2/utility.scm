;@file utility.scm
;Holds utility functions.

;Dependencies
;-var.scm

; Breaks a list to an expression for parsing.
(define decompose-list
    (lambda (_list)
        (cond
            ;empty list
            ((zero? (length _list))
                `(const ,@(list (void))))
            ;1 var list
            ((equal? 1 (length _list))
                (car _list))
            ;more than one
            (else
                `(begin ,@_list))
        )
    )
)

; Check all vars are with appropriate names
(define check-var-names
    (letrec
        ((check-var-names-loop
            (lambda (vars)
                (cond
                    ; Ended the list.
                    ((null? vars)
                        #t)
                    ((not (pair? vars))
                        #f)
                    ; recursive-call
                    ((_var? (car vars))
                        (check-var-names-loop (cdr vars)))
                    ; not a var name
                    (else
                        #f)
                )
            )
        ))
        (lambda (_list)
            (check-var-names-loop _list)
        )
    )
)

; Avoid double variable naming.
(define check-no-double-vars
    (letrec
        ((local-not-found
            (lambda (needle haystack)
                (cond
                    ((null? haystack)
                        ; not found! :)
                        #t)
                    ((not (list? haystack))
                        #f)
                    ((equal? needle (car haystack))
                        ; found the variable! sad!
                        #f)
                    (else
                        (local-not-found needle (cdr haystack))
                    )
                )
            )
        ))
        (lambda (input-list)
            (cond
                ((null? input-list)
                    #t)
                ; recursivly check for double variable names.
                ((local-not-found (car input-list) (cdr input-list))
                    (check-no-double-vars (cdr input-list)))
                (else
                    #f)
            )
        )
    )
)
