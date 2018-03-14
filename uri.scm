;; constants
;; golbal vars
;; set of golbal vars
;; get
;; seq
;; if, else
;; etc....
(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")
(load "builtins.scm")
;; ==================================
;; ====== COMPILER ==================
;; ==================================


;; Pipeline (sexpr or many as a list) -> parsed expression
(define pipeline
  (lambda (s)
    ((star <sexpr>) s
     (lambda (m r)
       (map (lambda (e)
              (annotate-tc
               (pe->lex-pe
                (box-set
                 (remove-applic-lambda-nil
                  (parse e))))))
            m))
     (lambda (f) 'fail))))

;; file->list ( input file ) -> sexpr
(define file->list
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
                (lambda ()
                  (let ((ch (read-char in-port)))
                    (if (eof-object? ch)
                        (begin
                          (close-input-port in-port)
                          '())
                        (cons ch (run)))))))
        (run)))))
;; =====================
;; create free var table
;; =====================
(define fvar-lookup
  (lambda (fv-table x)
    (caar (filter (lambda (line) (equal? x (cadr line))) fv-table))))

(define make-fvar-list
  (lambda (pe)
    (cond ((not (pair? pe))
           '())
          ((and (list? pe) (= 1 (length pe)))
           (make-fvar-list (car pe)))
          ((eqv? (car pe) 'fvar)
           `(,(cadr pe)))
          (else (append (make-fvar-list (car pe))
                        (make-fvar-list (cdr pe)))))))

(define make-fvar-table
  (lambda (pe)
    (letrec ((make (lambda (table fvars count)
                     (if (not (null? fvars))
                         (make `(,@table (,count ,(car fvars))) (cdr fvars) (+ 1 count))
                         table))))
      (make '() (list->set (make-fvar-list pe)) 0))))

(define fvar-assem
  (lambda (table)
    (if (null? table)
        ""
        (string-append "Lglob" (number->string (caar table)) ":\n\t"
                       "dq SOB_UNDEFINED\n"
                       (fvar-assem (cdr table))))))

;; ==============================================================
;; Code generator (parsed expressions, free var table) -> string of assembly code
;; ==============================================================
(define label-counter 0)

(define get-next-label
  (lambda ()
    (set! label-counter (+ 1 label-counter))
    label-counter))

;; TOOD- add rest of built ins
(define built-ins
  '((car "CAR") (cdr "CDR") (+ "PLUS")
    (boolean? "BOOL?") (< "LessThan") (> "MoreThan")
    (= "Equal=") (eq? "EQ") (char->integer "CHAR-TO-INT")
    (integer->char "INT-TO-CHAR") (cons "CONS")))

(define look-up-macro
  (lambda (x macros)
    (if (null? macros)
        #f
        (if (eqv? (caar macros) x)
            (cadar macros)
            (look-up-macro x (cdr macros))))))

;; loop1 for lambda closure
;(define l1
;  (lambda (m params)
;    (if (= 0 (length params))
;        ""
;        (string-append "mov qword[rcx+8*" (number->string m) "],"))))

(define code-gen
  (lambda (pe fv-table curr-lam-depth const-table)
    (cond ((not (pair? pe))
           "not-pair") ;; TODO; "mov rax,SOB_VOID\n"
          ((= 1 (length pe))
           (code-gen (car pe) fv-table curr-lam-depth const-table))
          ; DEFINE
          ((eqv? (car pe) 'define)
           (string-append (code-gen (caddr pe) fv-table curr-lam-depth const-table)
                          "mov [Lglob" (number->string (fvar-lookup fv-table (cadadr pe))) "],rax\n"
                          "mov rax,SOB_VOID\n"))
          ; SET
          ((eqv? (car pe) 'set)
           (string-append (code-gen (caddr pe) fv-table curr-lam-depth const-table)
                          "mov [Lglob" (number->string (fvar-lookup fv-table (cadadr pe))) "],rax\n"
                          "mov rax,SOB_VOID\n"))
          ; BOX-GET TODO

          ; CONST
          ((eqv? (car pe) 'const)
           (cond ((integer? (cadr pe))
                  (string-append "mov rax," (lookup-label (cadr pe) const-table) "\n"))
                 ((rational? (cadr pe))
                  (string-append "mov rax," (lookup-label (cadr pe) const-table) "\n"))
                 ((char? (cadr pe))
                  (string-append "mov rax," (lookup-label (cadr pe) const-table) "\n"))
                 ((boolean? (cadr pe))
                  (if (cadr pe)
                      "mov rax,SobT\n"
                      "mov rax,SobF\n"))
                 ((symbol? (cadr pe))
                  (string-append "mov rax,TODO-const-symbol" (symbol->string (cadr pe))))
                 ((or (list? (cadr pe))
                      (pair? (cadr pe)))
                  (string-append "mov rax," (lookup-label (cadr pe) const-table) "\n" ))
                 ((string? (cadr pe))
                  (string-append "mov rax," (lookup-label (cadr pe) const-table) "\n" ))
                 (else (string-append "TODO-const"))))
          ; FREE VAR ;; TODO first check if fvar is in fvar table and only if not take it from builtins
          ((eqv? (car pe) 'fvar)
           (let ((curr-macro (look-up-macro (cadr pe) built-ins))
                 (func-label (number->string (get-next-label))))
             (if curr-macro
                 (built-ins-string curr-macro curr-lam-depth func-label)
;                                ;; TODO: continue like lambda (for now without env) and use a macro for this
;                                ;;        at the end we return a closure with a label for the code
;                                ;;           the code will assume there is one arg in stack and will do CAR on it
                 (string-append "mov rax,[Lglob"
                          (number->string (fvar-lookup fv-table (cadr pe)))
                          "]\n"))))
          ; PVAR
          ((eqv? (car pe) 'pvar)
           (string-append "mov rax,qword[rbp+((4+" (number->string (caddr pe)) ")*8)]\n"))
          ; BVAR
          ((eqv? (car pe) 'bvar)
           (string-append "mov rax,qword[rbp+2*8]\n"
                          "mov rax,qword[rax+" (number->string (caddr pe)) "*8]\n"
                          "mov rax,qword[rax+" (number->string (cadddr pe)) "*8]\n"))
          ; SEQUENCE
          ((eqv? (car pe) 'seq)
           (fold-left string-append "" (map-by-order (lambda (x) (code-gen x fv-table curr-lam-depth const-table)) (cdr pe))))
          ; IF
          ((eqv? (car pe) 'if3)
           (let ((temp-label (number->string (get-next-label))))
             (string-append (code-gen (cadr pe) fv-table curr-lam-depth const-table)
                            "shr rax,4\n"
                            "cmp rax,0\n"
                            "je if_else" temp-label "\n"
                            (code-gen (caddr pe) fv-table curr-lam-depth const-table)
                            "jmp if_done" temp-label "\n"
                            "if_else" temp-label ":\n"
                            (code-gen (cadddr pe) fv-table curr-lam-depth const-table)
                            "if_done" temp-label ":\n")))
          ;OR (USE OF RDX)
          ((eqv? (car pe) 'or)
           (let ((or-number (number->string (get-next-label))))
             (letrec ((code-gen-or (lambda (lst)
                                     (if (null? lst)
                                       (string-append "mov rax,SOB_FALSE\n"
                                                      "end_of_or" or-number ":\n")
                                       (string-append (code-gen (car lst) fv-table curr-lam-depth const-table)
                                                      "push rdx\n"
                                                      "mov rdx,rax\n"
                                                      "shr rdx,4\n"
                                                      "cmp rdx,0\n"
                                                      "pop rdx\n"
                                                      "jne end_of_or" or-number "\n"
                                                      (code-gen-or (cdr lst)))))))
               (code-gen-or (cadr pe)))))
          ; LAMBDA (number->string (length (cadr pe)))
          ((eqv? (car pe) 'lambda-simple)
           (let ((curr_lam (number->string (get-next-label))))
             (string-append
                          "push rax\n" ;; rbx <- malloc(8*(n+1))
                          "mov rdi,8*" (number->string (+ 1 curr-lam-depth)) "\n"
                          "call malloc\n" ;; puts the address of the new space in RAX
                          "mov rbx,rax\n"
                          "pop rax\n"
                          ;; rcx <- malloc(8*m)
                          "mov r11,0\n"
                          "cmp r11," (number->string curr-lam-depth) "\n"
                          "je not_nested" curr_lam "\n"
                          "mov r11, qword[rbp+(3*8)]\n" ;; r11 <- number of params in prev lambda
                          "mov rdi,r11\n" ; params of previous lambda
                          "sal rdi,3\n" ;; multiply rdi by 8 (size of byte)
                          "call malloc\n"
                          "mov rcx,rax\n"
                          "mov r10,0\n" ;; i:=0
                          "params_loop" curr_lam ":\n\t"
                          "cmp r10,r11\n\t" ;; i<m
                          "je end_params_loop" curr_lam "\n\t"
                          "mov r12,qword[rbp+((4+r10)*8)]\n\t" ;; with the following line
                          "mov qword[rcx+(r10*8)],r12\n\t" ;; rcx[i]:=par[i]
                          "add r10,1\n\t" ;; i++
                          "jmp params_loop" curr_lam "\n"
                          "end_params_loop" curr_lam ":\n"
                          ;; env loop
                          "mov r12,qword[rbp+(2*8)]\n" ;; r12<-env
                          "mov r10,0\n" ;; i:=0
                          "mov r11,1\n" ;; j:=1
                          "env_loop" curr_lam ":\n\t"
                          "cmp r10," (number->string curr-lam-depth) "\n\t" ;; (i<n?)
                          "je end_env_loop" curr_lam "\n\t"
                          "mov r12,qword[r12+(r10*8)]\n\t" ;; with following line
                          "mov qword[rbx+r11],r12\n\t" ;; rbx[j]:=env[i]
                          "inc r10\n\t" ;; i++
                          "inc r11\n\t" ;; j++
                          "jmp env_loop" curr_lam "\n"
                          "end_env_loop" curr_lam ":\n"
                          "mov qword[rbx],rcx\n" ;; rbx[0] := rcx
                          "not_nested" curr_lam ":\n"

                          "mov rdi,8\n" ;; rax <- malloc(8)
                          "call malloc\n"
                          "jmp L" curr_lam "\n"
                          "code_lambda" curr_lam ":\n\t"
                          "push rbp\n\t"
                          "mov rbp,rsp\n\t"
                          (code-gen (caddr pe) fv-table (+ 1 curr-lam-depth) const-table)
                          "\tleave\n\t"
                          "ret\n"
                          "L" curr_lam ":\n\t"
                          ;"mov rdx,rax\n\t"
                          "MAKE_LITERAL_CLOSURE rax, rbx, code_lambda" curr_lam "\n\t"
                          ";mov rax,[rax]\n"))) ;;CHANGE1
          ; APPLIC
          ((eqv? (car pe) 'applic)
           (string-append ;; push args to stack
                          (fold-left string-append
                                     ""
                                     (map (lambda (arg) (string-append (code-gen arg fv-table curr-lam-depth const-table)
                                                            "push rax\n"))
                                                        (reverse (caddr pe))))
                          ;; push number of args to stack
                          "push " (number->string (length (caddr pe))) "\n"
                          ;; eval procedure (closure)
                          (code-gen (cadr pe) fv-table curr-lam-depth const-table)
                          ;; handle exception if not procedure
                          ;"mov r12,rax\n"
                          ;"TYPE r12\n"
                          ;"cmp r12,T_CLOSURE\n"
                          ;; push env to stack

                          "mov rax,[rax]\n" ;;CHANGE1

                          "mov rbx,rax\n"
                          "CLOSURE_ENV rbx\n"
                          "push rbx\n"
                          ;; apply procedure
                          "mov rbx,rax\n"
                          "CLOSURE_CODE rbx\n"
                          "call rbx\n"
                          ;; after applic - clean stack 2+num of args
                          "add rsp,(" (number->string (length (caddr pe))) "+2)*8 \n\t"
                          ))
          ((eqv? (car pe) 'tc-applic)
           (let ((curr_applic (number->string (get-next-label))))
             (string-append ;; push args to stack
                          (fold-left string-append
                                     ""
                                     (map (lambda (arg) (string-append (code-gen arg fv-table curr-lam-depth const-table)
                                                            "push rax\n"))
                                                        (reverse (caddr pe))))
                          ;; push number of args to stack
                          "push " (number->string (length (caddr pe))) "\n"
                          ;; eval procedure (closure)
                          (code-gen (cadr pe) fv-table curr-lam-depth const-table)
                          ;; push env to stack

                          "mov rax,[rax]\n" ;;CHANGE1

                          "mov rbx,rax\n"
                          "CLOSURE_ENV rbx\n"
                          "push rbx\n"
                          ;; override frame
                          "mov r8,qword[rbp+8]\n" ;; r8 <- ret (of prev lambda)
                          "push r8\n" ;; push ret
                          "mov r8,rbp\n" ;; backup for rbp
                          "mov rbp,qword[rbp]\n" ;; rbp receives old rbp from stack
                          ;; loop to override frame
                          "mov r13,qword[r8+8*3]\n" ;; r13 <- number of args on stack +3
                          "mov r14,rsp\n"
                          "add r13,4\n"
                          "mov r10,1\n" ;; i:=1
                          "mov r11," (number->string (+ 3 (length (caddr pe)))) "\n" ;; m + 3
                          "loop_override" curr_applic ":\n\t"
                          "cmp r10,r11\n\t"
                          "jg end_loop_override" curr_applic "\n\t"
                          "sub r11,r10\n\tsub r13,r10\n\t"
                          "mov r9,qword[r14+8*(r11)]\n\t" ;; src -> r9
                          "mov qword[r8+8*(r13)],r9\n\t" ;; r9 -> dest
                          "add r11,r10\n\tadd r13,r10\n\t"
                          "inc r10\n\t"
                          "jmp loop_override" curr_applic "\n"
                          "end_loop_override" curr_applic ":\n"
                          "mov rsp,r14\n" ;; update RSP after frame override
                          "mov rsp,r8\n"
                          "sal r13,3\n"
                          "add rsp,r13\n"
                          "sal r11,3\n"
                          "sub rsp,r11\n"
                          ;"mov rsp,r14+(8*r13)\n"
                          "CLOSURE_CODE rax\n"
                          "jmp rax\n"
                          )))
          (else (string-append (code-gen (car pe) fv-table curr-lam-depth const-table)
                               ;"\n"
                               (code-gen (cdr pe) fv-table curr-lam-depth const-table))))))

;; ========================
;; construct constant table
;; ========================
;; list->set (removes duplicates)
(define list->set
  (lambda (s)
    (fold-left (lambda (init-prev next) (if (ormap (lambda (si) (equal? next si)) init-prev)
                                 init-prev
                                 (cons next init-prev)))
               '()
               s)))

;; given a list returns a list containig all sub lists
(define sublists
  (lambda (l)
    (if (pair? l)
        `(,l ,(car l) ,@(sublists (cdr l)))
        `(,l))))

;; go over ast and find all constants
(define make-const-list
  (lambda (pe)
    (cond ((not (pair? pe))
           '())
          ((and (list? pe) (= 1 (length pe)))
           (make-const-list (car pe)))
          ((eqv? (car pe) 'const)
           (if (not (pair? (cadr pe)))
               `(,(cadr pe))
               ;; TODO: perhaps add here for fraction also the num and den
               (sublists (cadr pe))))
          (else (append (make-const-list (car pe))
                        (make-const-list (cdr pe)))))))

;; sort topologic (by length of const list)
(define consts-len
  (lambda (lst count)
    (if (not (null? (filter (lambda (a) (and (list? a) (= count (length a)))) lst)))
        (append (filter (lambda (a) (and (list? a) (= count (length a)))) lst)
                (consts-len lst (+ 1 count)))
        '())))

(define len-not-proper
  (lambda (notproper)
    (if (pair? notproper)
        (+ 1 (len-not-proper (cdr notproper)))
        1)))

(define sort-not-proper
  (lambda (lst count)
    (if (not (null? (filter (lambda (a) (and (pair? a)
                                             (not (list? a))
                                             (= count (len-not-proper a))))
                            lst)))
        (append (filter (lambda (a) (and (pair? a)
                                         (not (list? a))
                                         (= count (len-not-proper a))))
                        lst)
                (sort-not-proper lst (+ 1 count)))
        '())))

(define sort-consts
  (lambda (consts)
    (append (filter (lambda (a) (and (not (pair? a)) (not (list? a)))) consts)
            (sort-not-proper consts 2)
            (consts-len consts 0)
            )))



;; given a table with 3 columns and a line index (second element) in the table -
;;  find the line in the table comparing the first element to x
;;    and return the third element of the line
;; table = ((label val rep) ... (label val rep))
;; x = value in the table
(define rep
  (lambda (x table)
    (caddar (filter (lambda (line) (equal? x (cadr line)))
                    table))))

;; table = ((label val rep) ... (label val rep))
;; x = value in the table
(define lookup-label
  (lambda (x table)
    (caar (filter (lambda (line) (equal? x (cadr line)))
                    table))))


;; add label
(define add-const-label
  (lambda (x table)
    (cond ((integer? x)
           (if (< x 0)
               `(,(string-append "sobIntM" (number->string (abs x))) ,x ,(number->string x))
               `(,(string-append "sobInt" (number->string x)) ,x ,(number->string x))))
          ((rational? x)
           (if (< x 0)
               `(,(string-append "sobRationalM"
                                 (number->string (numerator (abs x)))
                                 "Div" (number->string (denominator x)))
                 ,x
                 ,(number->string x))
               `(,(string-append "sobRational"
                                 (number->string (numerator x))
                                 "Div"
                                 (number->string (denominator x)))
                 ,x
                 ,(number->string x))))
          ((boolean? x)
           (if x
               `("SobT" ,x "SOB_TRUE")
               `("SobF" ,x "SOB_FALSE")))
          ((list? x)
           (if (null? x)
               `("sobNil" ,x "N")
               `(,(string-append "sobPair" (rep (car x) table) (rep (cdr x) table))
                 ,x
                 ,(string-append (rep (car x) table) (rep (cdr x) table)))))
          ((pair? x)
           `(,(string-append "sobPair" (rep (car x) table) (rep (cdr x) table))
             ,x
             ,(string-append (rep (car x) table) (rep (cdr x) table))))
          ((string? x)
           `(,(string-append "sobStr" (number->string (get-next-label)))
             ,x
             ,x))
          ((char? x)
           `(,(string-append "sobChar" (number->string (char->integer x)))
             ,x
             ,(number->string (char->integer x))))
           )))

;; create const table
(define make-const-table
  (lambda (pe)
    (letrec ((make (lambda (table consts)
                     (if (not (null? consts))
                         ;(make (append table (add-label (car consts) table)) (cdr consts))
                         (make `(,@table ,(add-const-label (car consts) table)) (cdr consts))
                         table))))
       (make '() (sort-consts (reverse (list->set (make-const-list pe))))))))

; create const table in assembly
(define convert-const-to-assem
  (lambda (table)
    (fold-left (lambda (init-acc next)
                 (string-append init-acc next))
               ""
    (map (lambda (line)
           (cond ((integer? (cadr line))
                  (string-append (car line)
                                 ":\n\tdq MAKE_LITERAL(T_INTEGER, "
                                 (number->string (cadr line))
                                 ")\n"))
                 ((rational? (cadr line))
                  (string-append (car line)
                                 ":\n\tdq MAKE_LITERAL_FRAC("
                                 (string-append (number->string (numerator (cadr line))))
                                 ", "
                                 (string-append (number->string (denominator (cadr line))))
                                 ")\n"))
                 ((list? (cadr line))
                  (if (null? (cadr line))
                      "sobNil: \n\tdq SOB_NIL\n"
                      (string-append (car line)
                                     ":\n\tdq MAKE_LITERAL_PAIR("
                                     (lookup-label (caadr line) table)
                                     ", "
                                     (lookup-label (cdadr line) table)
                                     ")\n")))
                 ((pair? (cadr line))
                  (string-append (car line)
                                 ":\n\tdq MAKE_LITERAL_PAIR("
                                 (lookup-label (caadr line) table)
                                 ", "
                                 (lookup-label (cdadr line) table)
                                 ")\n"))
                 ((string? (cadr line))
                  (string-append (car line)
                                 ":\n\tMAKE_LITERAL_STRING \""
                                 (cadr line)
                                 "\"\n"))
                 ((char? (cadr line))
                  (string-append (car line)
                                 ":\n\tdq MAKE_LITERAL(T_CHAR, "
                                 (caddr line)
                                 ")\n"))
                 ((boolean? (cadr line))
                  (string-append (car line)
                                 ":\n\tdq "
                                 (caddr line)
                                 "\n"))))
         table))))

;; string support - receives bytes and returns number of qwords needed
(define qwords
  (lambda (bytes)
    (let ((divid (div bytes 8)))
      (if (= 0 (remainder bytes 8))
          divid
          (+ 1 divid)))))

;; THIS IS A TRY FOR initializing the constants after the start of program instead of in data section
(define convert-const-to-assem2
  (lambda (table)
    (fold-left (lambda (init-acc next)
                 (string-append init-acc next))
               ""
    (map (lambda (line)
           (cond ((integer? (cadr line))
                  (string-append "mov qword["(car line)"],"
                                 " MAKE_LITERAL(T_INTEGER, "
                                 (number->string (cadr line))
                                 ")\n"))
                 ((rational? (cadr line))
                  (string-append "mov rcx,"(string-append (number->string (numerator (cadr line))))"\n"
                                 "mov rdx,"(string-append (number->string (denominator (cadr line))))"\n"
                                 "MAKE_LITERAL_FRAC2 rcx,rdx ; num/den=>rcx \n"
                                 "mov qword["(car line)"],rcx \n"
                                 ))
                 ((list? (cadr line))
                  (if (null? (cadr line))
                      "mov qword[sobNil],SOB_NIL\n"
                      (string-append "mov rcx,"(lookup-label (caadr line) table)"\n"
                                     "mov rdx,"(lookup-label (cdadr line) table)"\n"
                                     "MAKE_LITERAL_PAIR rcx, rdx \n"
                                     "mov qword["(car line)"],rcx\n"
                                     )))
                 ((pair? (cadr line))
                  (string-append "mov rcx,"(lookup-label (caadr line) table)"\n"
                                 "mov rdx,"(lookup-label (cdadr line) table)"\n"
                                 "MAKE_LITERAL_PAIR rcx, rdx \n"
                                 "mov qword["(car line)"],rcx\n"
                                     ))
                 ((string? (cadr line))
                  (string-append "mov rcx,"(number->string (string-length (cadr line)))"\n" ; rcx:=length
                                 ;"mov rdx,"(car line)"\n" ; rdx:=address of literal string in bss
                                 ";rcx=length rdx=address of elements\n;now we insert the elements\n"

                                 "mov rbx, malloc_ptr ;get a pointer to mem\n\t"
                                 "mov rax, qword[rbx] \n\t"
                                 "add qword[rbx],(8*"(number->string (qwords (string-length (cadr line))))")\n\n\t"

                                 ;; for i=0 to length
                                 ;;    char->integer (string-ref str i) -> insert in next byte
                                 ;;       mov byte[rax] , ascii int
                                 (fold-left string-append
                                            ""
                                            (map (lambda (ch) (string-append "mov byte[rax],"
                                                                  (number->string (char->integer ch))
                                                                  "\n"
                                                                  "inc rax \n"))
                                                 (string->list (cadr line))))
                                 ";rdx should hold relative address\n"
                                 "mov rdx,rax \n"
                                 "sub rdx,start_of_data \n"
                                 "sal rdx,34 \n"
                                 "shr rdx,34 \n"
                                 "shr rcx,30 \n"
                                 "or rcx,rdx \n"
                                 "shr rcx,4 \n"
                                 "or rcx,T_STRING \n"
                                 "mov qword["(cadr line)"],rcx \n"
                                 ))
                                 ;":\n\tMAKE_LITERAL_STRING \""
                                 ;(cadr line)
                                 ;"\"\n"))
                 ((char? (cadr line))
                  (string-append (car line)
                                 ":\n\tdq MAKE_LITERAL(T_CHAR, "
                                 (caddr line)
                                 ")\n"))
                 ((boolean? (cadr line))
                  (string-append (car line)
                                 ":\n\tdq "
                                 (caddr line)
                                 "\n"))))
         table))))

;; ====================================
;; construct symbol table (linked-list)
;; ====================================
;; global var table
;; ================

;; ========================
(define compile-scheme-file
  (lambda (name-src name-target)
    (let ((p (open-output-file name-target))
          (data ";section .data \n;start_of_data:\n")
          (prolog (string-append "section .text \n"
                                 "main:\n"
                                 "push rbp\n"
                                 "mov rbp,rsp\n"
                                 "mov rax,malloc_ptr \n"
                                 "mov qword[rax],start_of_data \n"))
          (epilog "mov rax,[rax] \npush rax\ncall write_sob_if_not_void\nadd rsp, 1*8\nleave")
          (fvar-table (make-fvar-table (pipeline (file->list name-src))))
          (const-table (make-const-table (pipeline (file->list name-src)))))
      (if p
          (begin
           (display "%include 'macros2'\n" p)
           (display (string-append "section .bss\n"
                                   "malloc_ptr:\n\t"
                                   "resq 1\n"
                                   "start_of_data:\n\t"
                                   "resb 1048576\n") p) ;; TODO change number
           (display data p)
           ;; consts
           (display (convert-const-to-assem const-table) p)
           ;; free vars
           (display (fvar-assem fvar-table) p)
           (display prolog p)
           (newline p)
           ;; trying the bss-data fix
           (display (convert-const-to-assem2 const-table) p)
           (display (code-gen (pipeline (file->list name-src)) fvar-table 0 const-table) p)
           (newline p)
           (display epilog p))
          'error-opening-port)
      (close-output-port p))))

;    (call-with-output-file name-target
 ;     (lambda (output-port)
  ;      (display (code-gen (pipeline (file->list name-src))) output-port)))))
