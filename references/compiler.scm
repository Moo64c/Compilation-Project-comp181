(load "project/sexpr-parser.scm")
(load "project/tag-parser.scm")
(load "project/semantic-analyzer.scm")

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

(define file->list
	(lambda (input-file)
		(let ((in-port (open-input-file input-file)))
			(letrec ((run (lambda ()
							(let ((ch (read-char in-port)))
								(if (eof-object? ch)
									(begin (close-input-port in-port) '())
									(cons ch (run)))))))
				(run)))))

(define string->file
	(lambda (out-file str)
		(if (file-exists? out-file) (delete-file out-file))
		(let ((out-port (open-output-file out-file)))
			(letrec ((run
				(lambda (lst)
					(if (null? lst) (close-output-port out-port)
								(begin
									(write-char (car lst) out-port)
									(run (cdr lst)))))))
				(run (string->list str))))))

(define generate-label
	(lambda (label-prefix)
		(lambda ()
			(let ((index 0))
				(lambda ()
					(set! index (+ index 1))
					(string-append label-prefix (number->string index)))))))

(define exit_or_lbl ((generate-label "exit_or_")))
(define exit_if3_lbl ((generate-label "exit_if3_")))
(define else_if3_lbl ((generate-label "else_if3_")))
(define exit_app_lbl ((generate-label "exit_app_")))
(define skip_code_lbl ((generate-label "skip_code_")))
(define for_copy_args_lbl ((generate-label "for_copy_args_")))
(define end_of_copy_args_lbl ((generate-label "end_of_copy_args_")))
(define for_copy_envs_lbl ((generate-label "for_copy_envs_")))
(define end_of_copy_envs_lbl ((generate-label "end_of_copy_envs_")))
(define code_lbl ((generate-label "code_label_")))
(define new_env_lbl ((generate-label "new_env_")))
(define for_fix_stack_lbl ((generate-label "for_fix_stack_")))
(define end_of_fix_stack_lbl ((generate-label "end_of_fix_stack_")))
(define debug_lbl ((generate-label "DEBUG_")))
(define dont_push_lbl ((generate-label "dont_push_arg_")))
(define fvar_lbl ((generate-label "fvar_")))

(define to-string
	(lambda (exp)
		(cond ((number? exp) (number->string exp))
              ((symbol? exp) (symbol->string exp))
              (else exp))))

(define remove-duplicates-from-list
    (lambda (lst)
        (fold-left (lambda (acc curr)
                        (if (member curr acc)
                            acc
                            (append acc (list curr))))
                    '()
                    lst)))

(define str-list->delimited-str
	(lambda (lst)
		(if (= (length lst) 0) 
			""
			(fold-left (lambda (acc x) (string-append acc ", " x))
						(car lst)
						(cdr lst)))))

(define is-empty-string
	(lambda (str)
		(equal? str "")))

(define string->str-list 
	(lambda (str)
		(let* ((ans (fold-left (lambda (acc ch)
								(let ((curr-str (car acc)) (ans-acc (cdr acc)))
								(cond ((equal? ch #\nul) 
										(if (is-empty-string curr-str)
											(cons "" (append ans-acc (list "CHAR_NUL")))
											(cons "" (append ans-acc (list (string-append "\"" curr-str "\"")) (list "CHAR_NUL")))))
									  ((equal? ch #\tab)
										(if (is-empty-string curr-str)
											(cons "" (append ans-acc (list "CHAR_TAB")))
											(cons "" (append ans-acc (list (string-append "\"" curr-str "\"")) (list "CHAR_TAB")))))
									  ((equal? ch #\newline)
										(if (is-empty-string curr-str)
											(cons "" (append ans-acc (list "CHAR_NEWLINE")))
											(cons "" (append ans-acc (list (string-append "\"" curr-str "\"")) (list "CHAR_NEWLINE")))))
									  ((equal? ch #\return)
									  	(if (is-empty-string curr-str)
											(cons "" (append ans-acc (list "CHAR_RETURN")))
											(cons "" (append ans-acc (list (string-append "\"" curr-str "\"")) (list "CHAR_RETURN")))))
									  ((equal? ch #\space) 
									  	(if (is-empty-string curr-str)
											(cons "" (append ans-acc (list "CHAR_SPACE")))
											(cons "" (append ans-acc (list (string-append "\"" curr-str "\"")) (list "CHAR_SPACE")))))
									  (else 
											(cons (string-append curr-str (list->string (list ch))) ans-acc)))))

							  	(cons "" '())
							 	(string->list str))))
			(if (equal? (car ans) "")
				(cdr ans)
				(append (cdr ans) (list (string-append "\"" (car ans) "\""))))
		)
	))

(define lexical_env -1)

;------------------------LABEL-----------------------------------

(define label-start-index 1)

(define update-label
    (lambda (lbl-prefix)
        (let ((curr-index label-start-index))
            (set! label-start-index (+ label-start-index 1))
            (string-append lbl-prefix (number->string curr-index)))))

;------------------------CONST-TABLE------------------------------

(define const-table `(,(void) () ,#t ,#f))

(define expand-const-table
    (lambda (pe)
        (cond 	((or (null? pe) (not (list? pe))) #f)
              	((equal? (car pe) 'const)
              		(let ((val (cadr pe)))
	                	(cond  	((null? val) #f) 
	                		   	((vector? val)
	                        		(begin (vector-map (lambda (e) (expand-const-table `(const ,e))) val)
	                            	   	   (set! const-table (append const-table (list val)))))
	                      	   	((pair? val)
	                       		 	(begin (expand-const-table `(const ,(car val)))
	                               		   (expand-const-table `(const ,(cdr val)))
	                               		   (set! const-table (append const-table (list val)))))              
	                      	   	((and (number? val) (not (integer? val)))
	                                (let* ((original-num (numerator val))
	                                	   (original-den (denominator val))
	                                	   (gcd-val (gcd original-num original-den))
	                                       (updated-num (/ original-num gcd-val))
	                                       (updated-den (/ original-den gcd-val)))
	                                    (begin (expand-const-table `(const ,updated-num))
	                                           (expand-const-table `(const ,updated-den))
	                                           (set! const-table (append const-table (list val))))))
	                          	((symbol? val) 
	                          		(begin 	(set! const-table (append const-table (list (symbol->string val))))
		                                    (set! const-table (append const-table (list val))))) 
		                      	(else (set! const-table (append const-table (list val)))))))
                      
              	(else (map expand-const-table pe)))))
                   
(define remove-duplicates-from-const-table
    (lambda ()
        (set! const-table (remove-duplicates-from-list const-table))))

(define get-label-from-const-table
    (lambda(constant tagged-table)
        (let* ((curr-row (car tagged-table))
        	   (curr-val (cadr curr-row))
        	   (curr-label (car curr-row)))
        	  (if (equal? constant curr-val)
        	  		curr-label
        	  	  (get-label-from-const-table constant (cdr tagged-table))))))
                    
(define const-table-add-type-id
    (lambda()
        (set! const-table 
        	(fold-left
                (lambda (acc-table constant)
                	(let ((const-lbl "const_"))
                		(append acc-table 
			                    (list (cond 	
			                    		((equal? constant (void)) `(,(update-label const-lbl) ,constant T_VOID))
			                          	((equal? constant '()) `(,(update-label	const-lbl) ,constant T_NIL))
			                          	((equal? constant #t) `(,(update-label const-lbl) ,constant T_BOOL))
			                          	((equal? constant #f) `(,(update-label const-lbl) ,constant T_BOOL))
			                        	((number? constant)
			                            	(let* ((original-num (numerator constant))
			                                	   (original-den (denominator constant))
			                            		   (gcd-val (gcd original-num original-den))
			                                       (updated-num (/ original-num gcd-val))
			                                       (updated-den (/ original-den gcd-val)))
			                                (if (integer? constant)
			                                    `(,(update-label const-lbl) ,updated-num T_INTEGER)
			                                    `(,(update-label const-lbl) ,constant T_FRACTION))))
			                        	((char? constant) `(,(update-label const-lbl) ,constant T_CHAR))
				                        ((vector? constant)
				                                `(,(update-label const-lbl) ,constant T_VECTOR))
				                        ((string? constant)
				                                `(,(update-label const-lbl) ,constant T_STRING))
				                        ((symbol? constant)
				                            `(,(update-label const-lbl) ,constant T_SYMBOL))
				                        ((pair? constant)
				                            `(,(update-label const-lbl) ,constant T_PAIR))
				                        (else (error 'constant "const table add type id error")))))))
                '()
                const-table))))

(define create-const-table
	(lambda (pe-lst)
		(begin (expand-const-table pe-lst)
			   (remove-duplicates-from-const-table)
			   (const-table-add-type-id)
		)
    ))

(define get-const-string
	(lambda ()
		(fold-left
            (lambda (acc el)
            	(let ((const-val (cadr el))
					  (const-label (car el)))
                    (cond 	((equal? const-val (void))
								(string-append acc (string-append const-label ":\n\t dq SOB_VOID\n")))
				    	   	((equal? const-val '())
								(string-append acc (string-append const-label ":\n\t dq SOB_NIL\n")))
							((equal? const-val #t)
								(string-append acc (string-append const-label ":\n\t dq SOB_TRUE\n")))
							((equal? const-val #f)
								(string-append acc (string-append const-label ":\n\t dq SOB_FALSE\n")))
	                    	((number? const-val)
	                        	(let* ((original-num (numerator const-val))
	                        		   (original-den (denominator const-val))
	                        		   (gcd-val (gcd original-num original-den))
	                                   (updated-num (/ original-num gcd-val))
	                                   (updated-den (/ original-den gcd-val)))
	                            	(if (integer? const-val)
	                            		(string-append acc (string-append const-label ":\n\t dq MAKE_LITERAL(T_INTEGER, "(number->string updated-num)")\n"))
	                            		(string-append acc (string-append const-label ":\n\t dq MAKE_LITERAL_FRACTION("(get-label-from-const-table updated-num const-table) 
	                            																				  	  ", "(get-label-from-const-table updated-den const-table)")\n")))))
	                    	((char? const-val) 
								(string-append acc (string-append const-label ":\n\t dq MAKE_LITERAL(T_CHAR, "(number->string (char->integer const-val))")\n")))
	                        ((vector? const-val)
	                            (let ((label-lst (map (lambda (x) (get-label-from-const-table x const-table)) (vector->list const-val)))
	                            	  (vector-len (vector-length const-val)))
                        			(string-append acc (string-append const-label ":\n\t MAKE_LITERAL_VECTOR " (str-list->delimited-str label-lst)"\n"))))
	                        ((string? const-val)
                        		(string-append acc (string-append const-label ":\n\t MAKE_LITERAL_STRING " (str-list->delimited-str (string->str-list const-val))"\n")))
	                        ((symbol? const-val)
                        		(string-append acc (string-append const-label ":\n\t dq MAKE_LITERAL_SYMBOL("(get-label-from-const-table (symbol->string const-val) const-table)")\n")))
	                        ((pair? const-val)
	                        	(string-append acc (string-append const-label ":\n\t dq MAKE_LITERAL_PAIR("(get-label-from-const-table (car const-val) const-table)
	                            																	    ", "(get-label-from-const-table (cdr const-val) const-table)")\n")))
	                        (else (error 'constant "const table add address and type id error")))))
                ""
                const-table)))

;------------------------GLOBAL-VAR-TABLE------------------------------

(define init-global-var-table '((car "car") 
							   (cdr "cdr")
							   (cons "cons")
							   (list "list")
							   (null? "null?")
							   (boolean? "boolean?")
							   (pair? "pair?")
							   (char? "char?")
							   (integer? "integer?")
							   (procedure? "procedure?")
							   (string? "string?")
							   (symbol? "symbol?")
							   (vector? "vector?")
							   (zero? "zero?")
							   (not "not")
							   (car "car")
							   (cdr "cdr")
							   (number? "number?")
							   (rational? "rational?") 
							   (eq? "eq?")
							   (char->integer "char_to_integer")
							   (integer->char "integer_to_char")
							   (numerator "numerator")
							   (denominator "denominator")
							   (string-length "string_length")
							   (vector-length "vector_length")
							   (string-ref "string_ref")
							   (vector-ref "vector_ref")
							   (remainder "remainder")
							   (bin_equal "bin_equal")
							   (bin_plus  "bin_plus")
							   (bin_minus "bin_minus")
							   (bin_mul   "bin_mul")
							   (bin_div   "bin_div")
							   (bin_less_than   "bin_less_than")
							   (bin_greater_than   "bin_greater_than")
							   (string-set! "string_set")
							   (vector-set! "vector_set")
							   (make-string "make_string")
							   (make-vector "make_vector")
							   (vector "custom_vector")
							   (+ "plus")
							   (- "minus")
							   (/ "div")
							   (* "mul")
							   (< "less_than")
							   (> "greater_than")
							   (= "equal")
							   (map "map")
							   (apply "apply")
							   (symbol->string "symbol_to_string")
							   (string->symbol "string_to_symbol")))

(define global-var-table init-global-var-table)

(define expand-global-var-table
    (lambda (pe)
        (cond ((or (not (list? pe)) (null? pe)) #f)
              ((and (equal? (car pe) 'fvar) (not (assq (cadr pe) init-global-var-table)))
                (set! global-var-table (append global-var-table (list (list (cadr pe) (fvar_lbl))))))
              (else (map expand-global-var-table pe)))))

(define remove-duplicates-from-global-var-table
    (lambda ()
        (set! global-var-table (remove-duplicates-from-list global-var-table))))

(define get-label-from-global-var-table
    (lambda (val curr-var-table)
        (let* ((curr-row (car curr-var-table))
        	   (curr-val (car curr-row))
        	   (curr-label (cadr curr-row)))
        	  (if (equal? val curr-val)
        	  		curr-label
        	  	  (get-label-from-global-var-table val (cdr curr-var-table))))))

(define create-global-var-table
	(lambda (pe-lst)
		(begin (expand-global-var-table pe-lst)
			   (remove-duplicates-from-global-var-table)
			   ;(global-var-table-add-address)
			)))

(define get-fvar-string
	(lambda ()
		(fold-left
	        (lambda (acc el)
	            	(string-append acc (cadr el) ":\n\tdq SOB_UNDEFINED\n"))
	        ""
	        global-var-table)))

;------------------------SYMBOL-TABLE------------------------------

(define count-symbols
	(lambda ()
		(fold-left 
    		(lambda (acc el)
    			(if (equal? (caddr el) 'T_SYMBOL)
    				(+ acc 1)
    				acc))
    		0
    		const-table)))



(define gen-symbol-table 
    (lambda (curr-const-table index number-of-symbols-left)
        (if (equal? number-of-symbols-left 0)
        	(if (equal? index 0)
                (string-append "symbol_table:\n\t dq const_2\n") 
            	(string-append "symbol_table:\n\t dq symbol_0\n"))
            (if (not (null? curr-const-table))
            	(let* ((el (car curr-const-table)) 
    		  		  (const-label (car el)) 
    		  		  (const-val (cadr el)) 
    		  		  (const-type (caddr el)))
                	(if (not (equal? const-type 'T_SYMBOL))
                    	(gen-symbol-table (cdr curr-const-table) index number-of-symbols-left)
                    	(if (equal? number-of-symbols-left 1)
							(string-append "symbol_"(number->string index)": 
								\n\t dq MAKE_LITERAL_PAIR("const-label", const_2)\n" (gen-symbol-table (cdr curr-const-table) (+ index 1) (- number-of-symbols-left 1)))
							(string-append "symbol_"(number->string index)": 
								\n\t dq MAKE_LITERAL_PAIR("const-label", symbol_"(number->string (+ index 1))")\n"(gen-symbol-table (cdr curr-const-table) (+ index 1) (- number-of-symbols-left 1))))))
            (gen-symbol-table (cdr const-table) index number-of-symbols-left)))))


;------------------------CODE-GEN------------------------------

(define code-gen
    (lambda (pe)
        (cond 
            ((equal? (car pe) 'if3) (gen-if  pe))
            ((equal? (car pe) 'seq) (gen-seq pe))
            ((equal? (car pe) 'or)  (gen-or  pe))
            ((equal? (car pe) 'applic)  (gen-app pe))
           	((equal? (car pe) 'tc-applic)  (gen-app pe))
           	((equal? (car pe) 'lambda-simple)  (gen-lambda-simple pe))
            ((equal? (car pe) 'lambda-opt)  (gen-lambda-opt pe))
            ((equal? (car pe) 'define) (gen-define pe))
            ((equal? (car pe) 'const) (gen-const pe))
            ((equal? (car pe) 'fvar)  (gen-fvar pe))
            ((equal? (car pe) 'bvar) (gen-bvar pe))
            ((equal? (car pe) 'pvar) (gen-pvar pe))
            ((equal? (car pe) 'set) (gen-set pe))
            ((equal? (car pe) 'box) (gen-box pe))
            ((equal? (car pe) 'box-set) (gen-box-set pe))
            ((equal? (car pe) 'box-get) (gen-box-get pe))
            (else "mov rax, const_4\n")
        )))

(define gen-const
    (lambda(pe)
        (string-append "mov rax, "(to-string (get-label-from-const-table (cadr pe) const-table))"\n")))

(define gen-fvar
	(lambda(pe)
		(string-append "mov rax, ["(get-label-from-global-var-table (cadr pe) global-var-table)"]\n")))

(define gen-define
    (lambda(pe)
    	(let ((var-name (cadadr pe)) (pe-val (caddr pe)) (void_lbl "const_1"))
	        (string-append
	            (code-gen pe-val)
	            "mov [" (cadr (assq var-name global-var-table)) "], rax\n"
	            "mov rax, "void_lbl"\n"
	        ))))

(define gen-or
    (lambda (pe)
        (let ((exit_lbl (exit_or_lbl)) (first_arg (caadr pe)) (rest_args (cdadr pe)))
            (string-append
                (code-gen first_arg)
                (fold-right
                    string-append
                    ""
                    (map (lambda (next-el)
                            (string-append "cmp rax , const_4\n"
                                           "jne " exit_lbl "\n"
                                           (code-gen next-el)))
                    		rest_args))
                exit_lbl ":\n"))))

(define gen-if
    (lambda (pe)
        (let ((exit_lbl (exit_if3_lbl)) (else_lbl (else_if3_lbl)) (test_if3 (cadr pe)) (then_if3 (caddr pe)) (else_if3 (cadddr pe)))
            (string-append
                (code-gen test_if3)
                "cmp rax, const_4\n"
                "je "else_lbl"\n"
                (code-gen then_if3)
                "jmp "exit_lbl"\n"
                else_lbl":\n"
                (code-gen else_if3)
                exit_lbl":\n"))))

(define gen-seq
    (lambda (pe)
        (fold-left
            string-append
            ""
            (map code-gen (cadr pe)))))

(define gen-app
    (lambda (pe)
        (let ((args (reverse (caddr pe))) (proc (cadr pe)) (exit_lbl (exit_app_lbl)))
        (string-append
        	;"push const_2\n"
            (fold-left
                string-append
                ""
                (map (lambda (el)
                    (string-append
                        (code-gen el)
                        "push rax\n"))
                 args))
            "push "(number->string (length args))"\n"
			(code-gen proc)
			"mov rax, [rax]\n"
			"mov rbx, rax\n"
			"TYPE rbx\n"

            "cmp rbx, T_CLOSURE\n"
            "jne "exit_lbl"\n"

            "mov rbx, rax\n"            
			"CLOSURE_ENV rbx\n"
			"push rbx\n"			;push env
            "CLOSURE_CODE rax\n"
            "call rax\n"
            exit_lbl":\n"
            "add rsp, " (number->string (* 8 (+ 2 (length args))) ) "\n"))))

(define gen-tc-app
    (lambda (pe)
        (let ((args (reverse (caddr pe))) (proc (cadr pe)) (exit_lbl (exit_app_lbl)) (for_copy_args (for_copy_args_lbl)) (end_of_copy_args (end_of_copy_args_lbl)))
        (string-append
        	;"push const_2\n"
            (fold-left
                string-append
                ""
                (map (lambda (el)
                    (string-append
                        (code-gen el)
                        "push rax\n"))
                 args))
            "push "(number->string (length args))"\n"
			(code-gen proc)
			"mov rax, [rax]\n"
			"mov rbx, rax\n"
			"TYPE rbx\n"
            "cmp rbx, T_CLOSURE\n"
            "jne "exit_lbl"\n"

            "mov rbx, rax\n"            
			"CLOSURE_ENV rbx\n"
			"push rbx\n"
			
			"push ret_addr\n" ;save return address
			"mov r8, rbp\n"
			"mov rbp, qword[r8]\n"
			"mov r11,rsp\n"

			"mov r15,arg_count\n"
			"add r15, 5\n" 

			;copy arguments into rbp
			"mov rdi, "(number->string (+ 4 (length args))) "\n"
			for_copy_args":\n"
			"cmp rdi, 0\n"
			"je "end_of_copy_args"\n"

			"mov r12, rdi\n"
			"dec r12\n"
			"shl r12, 3\n"

			"mov r10,qword[r11+r12]\n"
			"dec r15\n"
			"mov r12,r15\n"
			"shl r12, 3\n"
			"mov qword[r8+r12],r10\n"
			"dec rdi\n"
			
			"jmp "for_copy_args"\n"
			end_of_copy_args":\n"

			"mov r12,r15\n"
			"shl r12, 3\n"
			"lea rsp,[r8+r12]\n"

            "CLOSURE_CODE rax\n"
            "jmp rax\n"

            exit_lbl":\n"
            ;"add rsp, " (number->string (* 8 (+ 3 (length args))) ) "\n"
        	))))


(define gen-lambda-simple
    (lambda (pe)
        (set! lexical_env (+ lexical_env 1))
            (let* ((args (cadr pe)) (body (caddr pe))
            	  (skip_code_label (skip_code_lbl)) (for_copy_args (for_copy_args_lbl)) (end_of_copy_args (end_of_copy_args_lbl))
            	  (for_copy_envs (for_copy_envs_lbl)) (end_of_copy_envs (end_of_copy_envs_lbl)) (code_label (code_lbl)) (new_env (new_env_lbl))
                  (str-gen (string-append
                  	;create new env
                  	"mov rbx, 0\n";env
                    "mov rax, " (number->string lexical_env) "\n";major
                    "cmp rax, 0\n"
                    "je "end_of_copy_envs"\n"
                    "mov rdi, "(number->string (* 8 (+ 1 lexical_env)))"\n";for allocating space for new extended env 
                    "call malloc\n"
                    "mov rbx, rax\n"	;rbx = malloc(8*(n+1)) **this is x**
                    
                    "mov rax, arg_count\n"
					"mov rdi, 8\n"
					"mul rdi\n"
                    "push rbx\n"	;save value of rbx 
                    "mov rdi, rax\n"
                    "call malloc\n"
                    "pop rbx\n"
                    "mov rcx, rax\n"	;rcx = malloc(8*m) **params of lambda**

                    ;copy arguments into rcx
					"mov rdi, 0\n"
					for_copy_args":\n"
					"cmp rdi, arg_count\n"
					"je "end_of_copy_args"\n"
					"mov rax, 8\n"
					"mul rdi\n"
					"mov rdx, An(rdi)\n"   ; rdx = i'th argument
					"mov qword [rcx+rax], rdx\n" ; copy arg i into [rcx+8*i]
					"inc rdi\n"
					"jmp "for_copy_args"\n"
					end_of_copy_args":\n"

					"mov qword [rbx], rcx\n"

					"mov r14, env\n"		; rdx=previous env
					"cmp r14, 0\n"
					"je "end_of_copy_envs"\n"
					"mov rdi, 0\n"
					for_copy_envs":\n"
					"cmp rdi, " (number->string lexical_env) "\n"
					"je "end_of_copy_envs"\n"
					"mov rax, 8\n"
					"mul rdi\n"
					"mov rcx, qword [r14+rax]\n" ; rcx = i'th env
					"mov qword [rbx+rax+8], rcx\n" ; copy env i into [rbx+8*i+8]
					"inc rdi\n"
					"jmp "for_copy_envs"\n"
					end_of_copy_envs":\n"

                    ;create target
                    "push rbx\n"
                    "push rcx\n"
                    "mov rdi, 16\n"
                    "call malloc\n" ;rax = malloc(8*2)
                    "pop rcx\n"
                    "pop rbx\n"

                    "MAKE_LITERAL_CLOSURE rax, rbx, " code_label "\n"

                    "jmp "skip_code_label"\n"
					;create code
					code_label":\n"
					"push rbp\n"
					"mov rbp, rsp\n"
					(code-gen body)
					"mov rbx, rax\n"
					"mov rax, arg_count\n"
					"add rax, 1\n"
					"mov rdi, 8\n"
					"mul rdi\n"
					"add rsp, rax\n"
					"mov rax, rbx\n"
					"leave\n"
					"ret\n"
					skip_code_label":\n")))
        		(set! lexical_env (- lexical_env 1)) 
        		str-gen)))

(define gen-lambda-opt
    (lambda (pe)
        (set! lexical_env (+ lexical_env 1))
            (let* ((args (cadr pe))
            	   (body (cadddr pe))
            	   (skip_code_label (skip_code_lbl)) (for_copy_args (for_copy_args_lbl)) (end_of_copy_args (end_of_copy_args_lbl))
            	   (for_copy_envs (for_copy_envs_lbl)) (end_of_copy_envs (end_of_copy_envs_lbl)) (code_label (code_lbl)) (new_env (new_env_lbl))
            	   (for_fix_stack (for_fix_stack_lbl)) (end_of_fix_stack (end_of_fix_stack_lbl)) (dont_push_arg_label (dont_push_lbl))
                   (str-gen (string-append
                  	;create new env
                  	"mov rbx, 0\n";env
                    "mov rax, " (number->string lexical_env) "\n";major
                    "cmp rax, 0\n"
                    "je "end_of_copy_envs"\n"
                    "mov rdi, "(number->string (* 8 (+ 1 lexical_env)))"\n";for allocating space for new extended env 
                    "call malloc\n"
                    "mov rbx, rax\n"	;rbx = malloc(8*(n+1)) **this is x**
                    
                    "mov rax, arg_count\n"
					"mov rdi, 8\n"
					"mul rdi\n"
                    "push rbx\n"	;save value of rbx 
                    "mov rdi, rax\n"
                    "call malloc\n"
                    "pop rbx\n"
                    "mov rcx, rax\n"	;rcx = malloc(8*m) **params of lambda**

                    ;copy arguments into rcx
					"mov rdi, 0\n"
					for_copy_args":\n"
					"cmp rdi, arg_count\n"
					"je "end_of_copy_args"\n"
					"mov rax, 8\n"
					"mul rdi\n"
					"mov rdx, An(rdi)\n"   ; rdx = i'th argument
					"mov qword [rcx+rax], rdx\n" ; copy arg i into [rcx+8*i]
					"inc rdi\n"
					"jmp "for_copy_args"\n"
					end_of_copy_args":\n"

					"mov qword [rbx], rcx\n"

					"mov r14, env\n"		; r14=previous env
					"cmp r14, 0\n"
					"jle "end_of_copy_envs"\n"
					"mov rdi, 0\n"
					for_copy_envs":\n"
					"cmp rdi, " (number->string lexical_env) "\n"
					"je "end_of_copy_envs"\n"
					"mov rax, 8\n"
					"mul rdi\n"
					"mov rcx, qword [r14+rax]\n" ; rcx = i'th env
					"mov qword [rbx+rax+8], rcx\n" ; copy env i into [rbx+8*i+8]
					"inc rdi\n"
					"jmp "for_copy_envs"\n"
					
					end_of_copy_envs":\n"
                    ;create target
                    "push rbx\n"
                    "push rcx\n"
                    "mov rdi, 16\n"
                    "call malloc\n" ;rax = malloc(8*2)
                    "pop rcx\n"
                    "pop rbx\n"

                    "MAKE_LITERAL_CLOSURE rax, rbx, " code_label "\n"

                    "jmp "skip_code_label"\n"

					code_label":\n"
					"push rbp\n"
					"mov rbp, rsp\n"
					"mov rbx, const_2\n"
					"mov r10, arg_count\n"

					for_fix_stack":\n"
					"cmp r10, "(number->string (length args)) "\n"
					"je " end_of_fix_stack "\n"
					
					"mov rdi, 8\n"
					"call malloc\n"			
					"mov rdx, rbp\n"				
					"add rdx, 4*8\n"				;rdx point to n+m in stack (offset)
					"mov r11, r10\n"				;r10 is helper for point of arg[i]
					"dec r11\n"
					"shl r11, 3\n"				;now offset+r10 = address of curr arg				
					"add rdx, r11\n"				;rdx = address of arg[i]
					"mov rdx, qword [rdx]\n"		
					
					"MAKE_MALLOC_LITERAL_PAIR rax, rdx, rbx\n"	;rax = target, rbx = cdr, rcx = car
					"mov rbx, rax\n"				;rbx ponints to the new pair as cdr for the new allocate in next iteration
					"dec r10\n"					
					"jmp " for_fix_stack "\n"
					
					end_of_fix_stack":\n"
					"cmp rbx, const_2\n"
					;"je "dont_push_arg_label"\n"
					"mov qword [rbp+4*8+"(number->string (length args))"*8], rbx\n"	;add the list in stack after const params (not optinals)
					;dont_push_arg_label":\n"
					;"mov qword [rbp+5*8+"(number->string (length args))"*8], const_2\n"
					;"mov qword [rbp + 3*8], "(number->string (+ 1 (length args)))"\n" ;update arg_count
					;"add rsp, r9\n"

					(code-gen body)
					
					"leave\n"
					"ret\n"
					skip_code_label":\n")))
        (set! lexical_env (- lexical_env 1)) 
	str-gen)))

(define gen-pvar
	(lambda (pe)
		(let ((minor (caddr pe)))
			(string-append "mov rax, qword [rbp+32+"(number->string minor)"*8]\n"))))

(define gen-bvar
	(lambda (pe)
		(let ((major (caddr pe)) (minor (cadddr pe)))
			(string-append 
			"mov rax, qword [rbp+16]\n"
			;"mov rax, env\n"
			"mov rax, qword [rax+"(number->string major)"*8]\n"
			"mov rax, qword [rax+"(number->string minor)"*8]\n"
			))))

(define gen-set
	(lambda (pe)
		(let ((tag (caadr pe))
               (var (cadadr pe))
               (val (caddr pe)))
			(cond ((equal? tag 'pvar) 
                    (let ((minor (car (cddadr pe))))
						(string-append
						(code-gen val)
						"mov qword [rbp+8*(4+"(number->string minor)")], rax\n"
						"mov rax, const_1\n"
						)))
                   ((equal? tag 'bvar)
                    (let ((major (car (cddadr pe)))
                         (minor (car(cdr (cddadr pe)))))
                            (string-append
                                (code-gen val)
                                "mov rbx, qword [rbp+16]\n"
                                "mov rbx, qword [rbx+8*"(number->string major)"]\n"
                                "mov qword [rbx+8*"(number->string minor)"], rax\n"
								"mov rax, const_1\n")))
                       ((equal? tag 'fvar) 
                            (string-append
                                (code-gen val)
                                "mov ["(get-label-from-global-var-table var global-var-table)"], rax\n"
                                "mov rax, const_1\n"
                                ))
                      (else "wrong input")))))

(define gen-box
	(lambda (pe)
		(let ((var (cadr pe)))
			(string-append
				(code-gen var)
				"mov rbx, rax\n"
				"mov rdi, 8\n"
				"call malloc\n"
				"mov qword [rax], rbx\n"
			))))

(define gen-box-get
	(lambda (pe)
		(let ((var (cadr pe)))
			(string-append
				(code-gen var)
				"mov qword rax, [rax]\n"
			))))

(define gen-box-set
	(lambda (pe)
		(let ((var (cadr pe)) (val (caddr pe)))
			(string-append
				(code-gen val)
				"mov rbx, rax\n"
				(code-gen var)
				"mov qword [rax], rbx\n"
				"mov rax, const_1\n"
			))))


;------------------------LIBRARY-FUNCTIONS------------------------------

;(define append-scheme-lib-functions
;	(lambda (lst) (append (string->list (string-append lib-fold-left lib-plus lib-map))
;								  lst)))

(define append-scheme-lib-functions
	(lambda (lst) (append (string->list (string-append lib-map lib-list lib-fold-left lib-bin-append lib-append 
				   						 				lib-equal lib-greater-than lib-less-than lib-less-than lib-plus lib-minus lib-mul lib-div ))
						  lst)))

(define lib-map "(define map (lambda (proc lst) (if (null? lst) 
													lst 
													(cons (proc (car lst)) (map proc (cdr lst))))))\n")

(define lib-list "(define list (lambda x x))\n")

(define lib-fold-left "(define fold_left (lambda (proc init lst) (if (null? lst) 
																	 init 
																	 (fold_left proc (proc init (car lst)) (cdr lst)))))\n")

(define lib-bin-append "(define bin_append (lambda (lst1 lst2) (if (null? lst1) 
																	lst2 
																	(cons (car lst1) (bin_append (cdr lst1) lst2)))))\n")

(define lib-append "(define append (lambda x (fold_left bin_append '() x)))\n")

(define lib-equal "(define = (lambda x (fold_left (lambda (acc y) (and acc (bin_equal (car x) y))) #t x)))\n")

(define lib-greater-than "(define > (lambda x (if (null? (cdr x))
												  #t 
												  (and (bin_greater_than (car x) (car (cdr x))) (apply > (cdr x)))))) \n")

(define lib-less-than "(define < (lambda x (if (null? (cdr x)) 
												#t 
												(and (bin_less_than (car x) (car (cdr x))) (apply < (cdr x)))))) \n")

(define lib-plus "(define + (lambda x (fold_left (lambda (acc y) (bin_plus acc y)) 0 x)))\n")

(define lib-minus "(define - (lambda x (if (null? (cdr x)) 
											(bin_minus 0 (car x)) 
											(fold_left (lambda (acc y) (bin_minus acc y)) (car x) (cdr x)))))\n")

(define lib-mul "(define * (lambda x (fold_left (lambda (acc y) (bin_mul acc y)) 1 x)))\n")

(define lib-div "(define / (lambda x (if (null? (cdr x)) 
										 (bin_div 1 (car x)) 
										 (fold_left (lambda (acc y) (bin_div acc y)) (car x) (cdr x)))))\n")

(define code-gen-library-functions
    (lambda ()
        (string-append
            (gen-cons)
            (gen-car)
            (gen-cdr)
            (gen-null?)
            (gen-pair?)
            (gen-boolean?)
            (gen-char?)
            (gen-integer?)
            (gen-procedure?)
            (gen-string?)
            (gen-symbol?)
            (gen-vector?)
            (gen-zero?)
            (gen-apply)
            (gen-make-string)
            (gen-make-vector)
            (gen-not)
            (gen-string-length)
            (gen-vector-length)
            (gen-vector)
            (gen-char->integer)
            (gen-integer->char)
            (gen-string-ref)
            (gen-vector-ref)
            (gen-symbol->string)
            (gen-string->symbol)
            (gen-bin-mul)
            (gen-bin-div)
            (gen-bin-minus)
            (gen-bin-equal)
            (gen-bin-less-than)
            (gen-bin-greater-than)
            (gen-string-set)
            (gen-vector-set)
            (gen-bin-plus)
            (gen-remainder)
            (gen-denominator)
            (gen-numerator)
            (gen-number?)
            (gen-rational?)
            (gen-eq?)
            )))

(define validate-type-code-gen

    (lambda (type-label tag)
    	(let ((lower-case-type-label (string-downcase type-label)))
	        (string-append
	           lower-case-type-label":\n"
	            "mov rdi, 16\n"
	            "call malloc\n"
	            "mov rbx, qword 0\n"
	            "MAKE_LITERAL_CLOSURE rax, rbx, "lower-case-type-label"_body\n"
	            "mov ["(symbol->string tag)"], rax\n"
	            "jmp "lower-case-type-label"_exit\n"

	            
	            lower-case-type-label"_body:\n"
	            "push rbp\n"
				"mov rbp, rsp\n"
	            "mov rbx, arg_count\n"
	            "cmp rbx, 1\n"
	 			"jne "lower-case-type-label"_exit\n"
	            "mov r10, An(0)\n"
	            "mov r10, [r10]\n"
	            "TYPE r10\n"
	            "cmp r10, T_"type-label"\n"
	            "jne "lower-case-type-label"_not\n"
	            "mov rax, const_3\n" 
	            "jmp "lower-case-type-label"_finish\n"
	            lower-case-type-label"_not:\n"
	            "mov rax, const_4\n"
	           	lower-case-type-label"_finish:\n"
	            "leave\n"
				"ret\n"
	            
	            lower-case-type-label"_exit:\n"))))
        
(define gen-null?
    (lambda()
        (validate-type-code-gen "NIL" 'null?)))

(define gen-pair?
    (lambda()
        (validate-type-code-gen "PAIR" 'pair?)))
        
(define gen-boolean?
    (lambda()
        (validate-type-code-gen "BOOL" 'boolean?)))
        
(define gen-char?
    (lambda()
        (validate-type-code-gen "CHAR" 'char?)))
        
(define gen-integer?
    (lambda()
        (validate-type-code-gen "INTEGER" 'integer?)))
        
(define gen-procedure?
    (lambda()
        (validate-type-code-gen "CLOSURE" 'procedure?)))
        
(define gen-string?
    (lambda()
        (validate-type-code-gen "STRING" 'string?)))
        
(define gen-symbol?
    (lambda()
        (validate-type-code-gen "SYMBOL" 'symbol?)))
        
(define gen-vector?
    (lambda()
        (validate-type-code-gen "VECTOR" 'vector?)))

(define gen-zero?
    (lambda()
        (string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, zero?_body\n"
            "mov [zero?], rax\n"
            "jmp zero?_exit\n"
            
            "zero?_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne zero_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "je zero_check\n"
	        "cmp rax, T_FRACTION\n"
	        "jne zero_finish\n"

	        "zero_check:\n"
	        "mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "DATA rax\n"
	        "cmp rax, 0\n"
	        "je zero_true\n"
	        "mov rax, const_4\n"
	        "jmp zero_finish\n"
	        "zero_true:\n"
	        "mov rax, const_3\n"

	        "zero_finish:\n"
	        "leave\n"
	        "ret\n"
	        "zero?_exit:\n" )))

(define gen-number?
    (lambda()
        (string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, number?_body\n"
            "mov [number?], rax\n"
            "jmp number?_exit\n"
            
            "number?_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne number?_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "je number?_true\n"
	        "cmp rax, T_FRACTION\n"
	        "je number?_true\n"
	        "mov rax, const_4\n"
	        "jmp number?_finish\n"
	        
	        "number?_true:\n"
	        "mov rax, const_3\n"

	        "number?_finish:\n"
	        "leave\n"
	        "ret\n"
	        "number?_exit:\n" )))

(define gen-not
    (lambda ()
        (string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, not_body\n"
            "mov [not], rax\n"
            "jmp not_exit\n"
            
            "not_body:\n"
            "push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" ; pop n from stack
	 		"jne not_exit\n"
            "mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "cmp rax, [const_4]\n"
	        "je is_false\n"
	        "mov rax, const_4\n"
	        "jmp not_finish\n"
	        "is_false:\n"
	        "mov rax, const_3\n"
	        "not_finish:\n"
	        "leave\n"
	        "ret\n"

            "not_exit:\n")))

(define gen-car
    (lambda()
        (string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, car_body\n"
            "mov [car], rax\n"
            "jmp car_exit\n"
            
            "car_body:\n"
            "push rbp\n"
			"mov rbp, rsp\n"       
            "mov rax, An(0)\n"
            "mov rax, [rax]\n"
            "DATA_UPPER rax\n"
			"add rax, start_of_data\n"
            "leave\n"
            "ret\n"
            
            "car_exit:\n")))

(define gen-cdr
    (lambda()
        (string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, cdr_body\n"
            "mov [cdr], rax\n"
            "jmp cdr_exit\n"
            
            "cdr_body:\n"
            "push rbp\n"
			"mov rbp, rsp\n"       
            "mov rax, An(0)\n"
            "mov rax, [rax]\n"
            "DATA_LOWER rax\n"
			"add rax, start_of_data\n"
            "leave\n"
            "ret\n"
            
            "cdr_exit:\n")))

(define gen-cons
    (lambda()
        (string-append
            
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, cons_body\n"
            "mov [cons], rax\n"
            "jmp cons_exit\n"
            
            "cons_body:\n"
            "push rbp\n"
			"mov rbp, rsp\n"
			"mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne cons_finish\n"
            "mov rdi, 8\n"
            "call malloc\n"
            "mov rcx, An(0)\n"
            "mov rdx, An(1)\n"
            "MAKE_MALLOC_LITERAL_PAIR rax, rcx, rdx\n"
            "cons_finish:\n"
        	"leave\n"
            "ret\n"
            
            "cons_exit:\n")))

(define gen-rational?
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, rational?_body\n"
            "mov [rational?], rax\n"
            "jmp rational?_exit\n"
            
            "rational?_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne rational?_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "je rational?_true\n"
	        "cmp rax, T_FRACTION\n"
	        "je rational?_true\n"
	        "mov rax, const_4\n"
	        "jmp rational?_finish\n"
	        
	        "rational?_true:\n"
	        "mov rax, const_3\n"

	        "rational?_finish:\n"
	        "leave\n"
	        "ret\n"
	        "rational?_exit:\n" )))

(define gen-eq?
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, eq?_body\n"
            "mov [eq?], rax\n"
            "jmp eq?_exit\n"
            
            "eq?_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne eq?_finish\n"

	 		"mov rax, An(0)\n"
	 		"mov rax, [rax]\n"
	 		"mov rbx, An(1)\n"
	 		"mov rbx, [rbx]\n"
	        "cmp rax, rbx\n"
	        "je eq?_true\n"
	        "mov rax, const_4\n"
	        "jmp eq?_finish\n"
	        
	        "eq?_true:\n"
	        "mov rax, const_3\n"

	        "eq?_finish:\n"
	        "leave\n"
	        "ret\n"
	        "eq?_exit:\n" )))

(define gen-bin-equal
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_equal_body\n"
            "mov [bin_equal], rax\n"
            "jmp bin_equal_exit\n"
            
            "bin_equal_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne bin_equal_finish\n"

	 		"mov rax, An(0)\n"
	 		"mov rax, [rax]\n"
	 		"mov rcx, rax\n"
	 		"mov rbx, An(1)\n"
	 		"mov rbx, [rbx]\n"
	 		"mov rdx, rbx\n"
	 		"TYPE rax\n"
	 		"TYPE rbx\n"
	        
	        "cmp rax, T_INTEGER\n"
	        "jne check_fraction\n"
	        "cmp rbx, T_INTEGER\n"
	        "jne bin_equal_false\n"
	        
	        "DATA rcx\n"
	        "DATA rdx\n"
	        "cmp rcx, rdx\n"
	        "jne bin_equal_false\n"
	        "mov rax, const_3\n"
	        "jmp bin_equal_finish\n"

	        "check_fraction:\n"
	        "cmp rax, T_FRACTION\n"
	        "jne bin_equal_finish\n"
	        "cmp rbx, T_FRACTION\n"
	        "jne bin_equal_false\n"

	        "mov r8, rcx\n"
	        "mov r9, rdx\n"
	        "CAR rcx\n"
	        "CAR rdx\n"
	        "cmp rcx, rdx\n"
	        "jne bin_equal_false\n"
	        "CDR r8\n"
	        "CDR r9\n"
	        "cmp r8, r9\n"
	        "jne bin_equal_false\n"
	        "mov rax, const_3\n"
	        "jmp bin_equal_finish\n"

	        "bin_equal_false:\n"
	        "mov rax, const_4\n"

	        "bin_equal_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_equal_exit:\n" )))

(define gen-bin-plus
	(lambda ()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_plus_body\n"
            "mov [bin_plus], rax\n"
            "jmp bin_plus_exit\n"
            
            "bin_plus_body:\n"
        	"push rbp\n"
      		"mov rbp, rsp\n"
      		"mov rcx ,arg_count\n"
      		"mov rax ,An(0)\n"                

      		"cmp rcx, 2\n"         			; nil + 1 arguments?
      		"jne bin_plus_finish\n"
     
      		"mov rax ,qword [rax]\n"                     
      		"mov rcx, rax\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"je bin_plus_arg1_int_check_arg2\n"
      
      		"bin_plus_arg1_frac_check_arg2:\n"
      		"mov rbx, An(1)\n"
       		"mov rbx, qword [rbx]\n"                    
      		"mov rcx, rbx\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"jne bin_plus_arg1_frac_arg2_frac\n"
      		"mov r10, rax\n"
      		"mov rax, rbx\n"
      		"mov rbx, r10\n"
      		"jmp bin_plus_arg1_int_arg2_frac\n"
      
      		"bin_plus_arg1_frac_arg2_frac:\n"
      		"mov rcx, rax\n"				
      		"CAR rcx\n"
      		"DATA rcx\n"					;rcx holds arg1 numerator	
      		"mov r11, rax\n"      			;rdx keeps rax value
      		"CDR r11\n"
      		"DATA r11\n"
      
     		"mov r10, rbx\n"					
      		"CAR r10\n"
      		"DATA r10\n"					;r10 holds arg2 numerator
      		"mov r12, rbx\n"				;r12 keeps r10 value
      		"CDR r12\n"
      		"DATA r12\n"
      		"mov r8, r11\n"					;backup first denominator

      		"MULT r11, r12\n"				;now r11 holds arg1 denominator * arg2 denominator
      		"MULT rcx, r12\n"				;now rcx holds arg1 numerator * arg2 denominator
      		"MULT r10, r8\n"				;now r10 holds second numerator * arg1 denominator
      		"add rcx, r10\n"
      		"mov rax, rcx\n"	
      		"mov r12, r11\n"					
      		"jmp bin_plus_simplify_and_create_fraction\n"                                             
      		
		  	"bin_plus_arg1_int_check_arg2:\n"
		  	"mov rbx, An(1)\n"
		  	"mov rbx, qword [rbx]\n"                    
		  	"mov rdx, rbx\n"
		  	"TYPE rdx\n"
		  	"cmp rdx, T_INTEGER\n"
		  	"je bin_plus_arg1_int_arg2_int\n"

		  	"bin_plus_arg1_int_arg2_frac:\n"
		  	"mov r10, rbx\n"
		  	"CAR r10\n"
		  	"DATA r10\n";we will put in r10 second_numerator
		  	"mov r12, rbx\n"
		  	"CDR r12\n"
		  	"DATA r12\n";we will put in r12 second_denominator
		  	"mov r8,rax\n"
		  	"DATA r8\n"
		  	"MULT r8,r12\n"
		  	"mov rax,r8\n";now rax holds first_numerator*second_denominator
		  	"add rax,r10\n"

		  	;rax holds new numerator
		  	;r12 holds new denominator
		  	"bin_plus_simplify_and_create_fraction:\n"
		  	"mov r8,  rax\n"
		  	"mov r9, r12\n"
		  	"push r9\n"
		  	"push r8\n"
		  	"call simplify_fraction\n"
		  	"add rsp, 16\n"
		  	"jmp bin_plus_create_new_fraction\n"

		  	"bin_plus_arg1_int_arg2_int:\n"
		  	"DATA rbx\n" 
		  	"DATA rax\n"      
		  	"add rbx,rax\n"
		  	"shl rbx, 4\n"
		  	"or rbx, T_INTEGER\n"
		  	"push rbx\n"
		  	"mov rdi, 8\n"
		  	"call malloc\n"
		  	"pop rbx\n"
		  	"mov [rax],rbx\n"
		                        
		  	"jmp bin_plus_finish\n"                     
                    
		  	"bin_plus_create_new_fraction:\n"
		  	"cmp r9, 1\n"
		  	"je bin_plus_create_new_int\n"

		  	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"		;r8 holds numerator in data structure
		  	"shl r9, 4\n"
		  	"or r9, T_INTEGER\n"		;r9 holds denominator in data structure
		    
		    "mov rdi, 8\n"
		    "call malloc\n"
		    "mov rbx, rax\n"
		    "mov [rbx], r8\n"
		    "push rbx\n"

		    "mov rdi, 8\n"
		    "call malloc\n"
		    "pop rbx\n"
		    "mov rcx, rax\n"
		    "mov [rcx], r9\n"
		    "push rbx\n"
		    "push rcx\n"

		    "mov rdi, 8\n"
		    "call malloc\n" 
		    "pop rcx\n"
		    "pop rbx\n"
                                               
		  	"MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx\n"
		  	"jmp bin_plus_finish\n"
		          
		   	"bin_plus_create_new_int:\n"

		   	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"
		  	"push r8\n"

		  	"mov rdi, 8\n"
		    "call malloc\n"
		    "pop r8\n"
		    "mov [rax], r8\n"
          
	        "bin_plus_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_plus_exit:\n")))

(define gen-bin-minus
	(lambda ()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_minus_body\n"
            "mov [bin_minus], rax\n"
            "jmp bin_minus_exit\n"
            
            "bin_minus_body:\n"
        	"push rbp\n"
      		"mov rbp, rsp\n"
      		"mov rcx ,arg_count\n"
      		"mov rax ,An(0)\n"                

      		"cmp rcx, 2\n"         			; nil + 1 arguments?
      		"jne bin_minus_finish\n"
     
      		"mov rax ,qword [rax]\n"                     
      		"mov rcx, rax\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"je bin_minus_arg1_int_check_arg2\n"
      
      		"bin_minus_arg1_frac_check_arg2:\n"
      		"mov rbx, An(1)\n"
       		"mov rbx, qword [rbx]\n"                    
      		"mov rcx, rbx\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"je bin_minus_arg1_frac_arg2_int\n"
   
      		"bin_minus_arg1_frac_arg2_frac:\n"
      		"mov rcx, rax\n"				
      		"CAR rcx\n"
      		"DATA rcx\n"					;rcx holds arg1 numerator	
      		"mov r11, rax\n"      			;rdx keeps rax value
      		"CDR r11\n"
      		"DATA r11\n"
      
     		"mov r10, rbx\n"					
      		"CAR r10\n"
      		"DATA r10\n"					;r10 holds arg2 numerator
      		"mov r12, rbx\n"				;r12 keeps r10 value
      		"CDR r12\n"
      		"DATA r12\n"
      		"mov r8, r11\n"					;backup first denominator

      		"MULT r11, r12\n"				;now r11 holds arg1 denominator * arg2 denominator
      		"MULT rcx, r12\n"				;now rcx holds arg1 numerator * arg2 denominator
      		"MULT r10, r8\n"				;now r10 holds second numerator * arg1 denominator
      		"sub rcx, r10\n"
      		"mov rax, rcx\n"	
      		"mov r12, r11\n"					
      		"jmp bin_minus_simplify_and_create_fraction\n"                                             
      		
		  	"bin_minus_arg1_int_check_arg2:\n"
		  	"mov rbx, An(1)\n"
		  	"mov rbx, qword [rbx]\n"                    
		  	"mov rdx, rbx\n"
		  	"TYPE rdx\n"
		  	"cmp rdx, T_INTEGER\n"
		  	"je bin_minus_arg1_int_arg2_int\n"

		  	"bin_minus_arg1_int_arg2_frac:\n"
		  	"mov r10, rbx\n"
		  	"CAR r10\n"
		  	"DATA r10\n";we will put in r10 second_numerator
		  	"mov r12, rbx\n"
		  	"CDR r12\n"
		  	"DATA r12\n";we will put in r12 second_denominator
		  	"mov r8,rax\n"
		  	"DATA r8\n"
		  	"MULT r8,r12\n"
		  	"mov rax,r8\n";now rax holds first_numerator*second_denominator
		  	"sub rax,r10\n"
		  	"jmp bin_minus_simplify_and_create_fraction\n"

		  	"bin_minus_arg1_frac_arg2_int:\n"
		  	"mov r10, rax\n"
		  	"CAR r10\n"
		  	"DATA r10\n"
		  	"mov r12, rax\n"
		  	"CDR r12\n"
		  	"DATA r12\n"
		  	"mov r8,rbx\n"
		  	"DATA r8\n"
		  	"MULT r8,r12\n"
		  	"mov rax,r8\n";now rax holds first_numerator*second_denominator
		  	"sub r10, rax\n"
		  	"mov rax, r10\n"

		  	;rax holds new numerator
		  	;r12 holds new denominator
		  	"bin_minus_simplify_and_create_fraction:\n"
		  	"mov r8,  rax\n"
		  	"mov r9, r12\n"
		  	"push r9\n"
		  	"push r8\n"
		  	"call simplify_fraction\n"
		  	"add rsp, 16\n"
		  	"jmp bin_minus_create_new_fraction\n"

		  	"bin_minus_arg1_int_arg2_int:\n"
		  	"DATA rbx\n" 
		  	"DATA rax\n"      
		  	"sub rax, rbx\n"
		  	"mov r10, rax\n"
		  	"shl r10, 4\n"
		  	"or r10, T_INTEGER\n"
		  	"push r10\n"
		  	"mov rdi, 8\n"
		  	"call malloc\n"
		  	"pop r10\n"
		  	"mov [rax],r10\n"
		                        
		  	"jmp bin_minus_finish\n"                     
                    
		  	"bin_minus_create_new_fraction:\n"
		  	"cmp r9, 1\n"
		  	"je bin_minus_create_new_int\n"

		  	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"		;r8 holds numerator in data structure
		  	"shl r9, 4\n"
		  	"or r9, T_INTEGER\n"		;r9 holds denominator in data structure
		    
		    "mov rdi, 8\n"
		    "call malloc\n"
		    "mov rbx, rax\n"
		    "mov [rbx], r8\n"
		    "push rbx\n"

		    "mov rdi, 8\n"
		    "call malloc\n"
		    "pop rbx\n"
		    "mov rcx, rax\n"
		    "mov [rcx], r9\n"
		    "push rbx\n"
		    "push rcx\n"

		    "mov rdi, 8\n"
		    "call malloc\n" 
		    "pop rcx\n"
		    "pop rbx\n"
                                              
		  	"MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx\n"      
		  	"jmp bin_minus_finish\n"
		          
		   	"bin_minus_create_new_int:\n"

		   	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"
		  	"push r8\n"

		  	"mov rdi, 8\n"
		    "call malloc\n"
		    "pop r8\n"
		    "mov [rax], r8\n"
          
	        "bin_minus_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_minus_exit:\n")))

(define gen-bin-mul
	(lambda ()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_mul_body\n"
            "mov [bin_mul], rax\n"
            "jmp bin_mul_exit\n"
            
            "bin_mul_body:\n"
        	"push rbp\n"
      		"mov rbp, rsp\n"
      		"mov rcx ,arg_count\n"
      		"mov rax ,An(0)\n"                

      		"cmp rcx, 2\n"         			; nil + 1 arguments?
      		"jne bin_mul_finish\n"
     
      		"mov rax ,qword [rax]\n"                     
      		"mov rcx, rax\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"je bin_mul_arg1_int_check_arg2\n"
      
      		"bin_mul_arg1_frac_check_arg2:\n"
      		"mov rbx, An(1)\n"
       		"mov rbx, qword [rbx]\n"                    
      		"mov rcx, rbx\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"jne bin_mul_arg1_frac_arg2_frac\n"
      		"mov r10, rax\n"
      		"mov rax, rbx\n"
      		"mov rbx, r10\n"
      		"jmp bin_mul_arg1_int_arg2_frac\n"
      
      		"bin_mul_arg1_frac_arg2_frac:\n"
      		"mov rcx, rax\n"				
      		"CAR rcx\n"
      		"DATA rcx\n"					;rcx holds arg1 numerator	
      		"mov r11, rax\n"      			;rdx keeps rax value
      		"CDR r11\n"
      		"DATA r11\n"
      
     		"mov r10, rbx\n"					
      		"CAR r10\n"
      		"DATA r10\n"					;r10 holds arg2 numerator
      		"mov r12, rbx\n"				;r12 keeps r10 value
      		"CDR r12\n"
      		"DATA r12\n"
      		"mov r8, r11\n"					;backup first denominator

      		"MULT r11, r12\n"				;now r11 holds arg1 denominator * arg2 denominator
      		"MULT r10, rcx\n"				;now r10 holds arg1 numerator * arg2 numerator
      		"mov rax, r10\n"	
      		"mov r12, r11\n"					
      		"jmp bin_mul_simplify_and_create_fraction\n"                                             
      		
		  	"bin_mul_arg1_int_check_arg2:\n"
		  	"mov rbx, An(1)\n"
		  	"mov rbx, qword [rbx]\n"                    
		  	"mov rdx, rbx\n"
		  	"TYPE rdx\n"
		  	"cmp rdx, T_INTEGER\n"
		  	"je bin_mul_arg1_int_arg2_int\n"

		  	"bin_mul_arg1_int_arg2_frac:\n"
		  	"mov r10, rbx\n"
		  	"CAR r10\n"
		  	"DATA r10\n";we will put in r10 second_numerator
		  	"mov r12, rbx\n"
		  	"CDR r12\n"
		  	"DATA r12\n";we will put in r12 second_denominator
		  	"mov r8,rax\n"
		  	"DATA r8\n"
		  	"MULT r8,r10\n"
		  	
		  	"mov rax,r8\n";now rax holds first_numerator*second_numerator

		  	;rax holds new numerator
		  	;r12 holds new denominator
		  	"bin_mul_simplify_and_create_fraction:\n"
		  	"mov r8,  rax\n"
		  	"mov r9, r12\n"
		  	"push r9\n"
		  	"push r8\n"
		  	"call simplify_fraction\n"
		  	"add rsp, 16\n"
		  	"jmp bin_mul_create_new_fraction\n"

		  	"bin_mul_arg1_int_arg2_int:\n"
		  	"DATA rbx\n" 
		  	"DATA rax\n"
		  	"mov r10, rax\n"     
		  	"MULT rbx,r10\n"
		  	"shl rbx, 4\n"
		  	"or rbx, T_INTEGER\n"
		  	"push rbx\n"
		  	"mov rdi, 8\n"
		  	"call malloc\n"
		  	"pop rbx\n"
		  	"mov [rax],rbx\n"
		                        
		  	"jmp bin_mul_finish\n"                     
                    
		  	"bin_mul_create_new_fraction:\n"
		  	"cmp r9, 1\n"
		  	"je bin_mul_create_new_int\n"

		  	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"		;r8 holds numerator in data structure
		  	"shl r9, 4\n"
		  	"or r9, T_INTEGER\n"		;r9 holds denominator in data structure
		    
		    "mov rdi, 8\n"
		    "call malloc\n"
		    "mov rbx, rax\n"
		    "mov [rbx], r8\n"
		    "push rbx\n"

		    "mov rdi, 8\n"
		    "call malloc\n"
		    "pop rbx\n"
		    "mov rcx, rax\n"
		    "mov [rcx], r9\n"
		    "push rbx\n"
		    "push rcx\n"

		    "mov rdi, 8\n"
		    "call malloc\n" 
		    "pop rcx\n"
		    "pop rbx\n"
                                               
		  	"MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx\n"
		  	"jmp bin_mul_finish\n"
		          
		   	"bin_mul_create_new_int:\n"

		   	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"
		  	"push r8\n"

		  	"mov rdi, 8\n"
		    "call malloc\n"
		    "pop r8\n"
		    "mov [rax], r8\n"
          
	        "bin_mul_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_mul_exit:\n")))

(define gen-bin-div
	(lambda ()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_div_body\n"
            "mov [bin_div], rax\n"
            "jmp bin_div_exit\n"
            
            "bin_div_body:\n"
        	"push rbp\n"
      		"mov rbp, rsp\n"
      		"mov rcx ,arg_count\n"
      		"mov rax ,An(0)\n"                

      		"cmp rcx, 2\n"         			; nil + 1 arguments?
      		"jne bin_div_finish\n"
     
      		"mov rax ,qword [rax]\n"                     
      		"mov rcx, rax\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"je bin_div_arg1_int_check_arg2\n"
      
      		"bin_div_arg1_frac_check_arg2:\n"
      		"mov rbx, An(1)\n"
       		"mov rbx, qword [rbx]\n"                    
      		"mov rcx, rbx\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"je bin_div_arg1_frac_arg2_int\n"
      
      		"bin_div_arg1_frac_arg2_frac:\n"
      		"mov rcx, rax\n"				
      		"CAR rcx\n"
      		"DATA rcx\n"					;rcx holds arg1 numerator	
      		"mov r11, rax\n"      			;rdx keeps rax value
      		"CDR r11\n"
      		"DATA r11\n"
      
     		"mov r10, rbx\n"					
      		"CAR r10\n"
      		"DATA r10\n"					;r10 holds arg2 numerator
      		"mov r12, rbx\n"				;r12 keeps r10 value
      		"CDR r12\n"
      		"DATA r12\n"
      		"mov r8, r11\n"					;backup first denominator

      		"MULT rcx, r12\n"				;now rcx holds arg1 numerator * arg2 denominator
      		"MULT r11, r10\n"				;now r11 holds arg1 denominator * arg2 numerator
      		"cmp r11, 0\n"
		  	"jg bin_div_positive_int1\n"
		  	"neg r11\n"
		  	"neg rcx\n"
		  	"bin_div_positive_int1:\n"
      		"mov rax, rcx\n"	
      		"mov r12, r11\n"					
      		"jmp bin_div_simplify_and_create_fraction\n"                                             
      		
		  	"bin_div_arg1_int_check_arg2:\n"
		  	"mov rbx, An(1)\n"
		  	"mov rbx, qword [rbx]\n"                    
		  	"mov rdx, rbx\n"
		  	"TYPE rdx\n"
		  	"cmp rdx, T_INTEGER\n"
		  	"je bin_div_arg1_int_arg2_int\n"

		  	"bin_div_arg1_int_arg2_frac:\n"
		  	"mov r10, rbx\n"
		  	"CAR r10\n"
		  	"DATA r10\n";we will put in r10 second_numerator
		  	"mov r12, rbx\n"
		  	"CDR r12\n"
		  	"DATA r12\n";we will put in r12 second_denominator
		  	"mov r8,rax\n"
		  	"DATA r8\n"
		  	"MULT r8,r12\n"
		  	"cmp r10, 0\n"
		  	"jg bin_div_positive_int2\n"
		  	"neg r10\n"
		  	"neg r8\n"
		  	"bin_div_positive_int2:\n"
		  	"mov r12, r10\n"
		  	"mov rax,r8\n"
		  	"jmp bin_div_simplify_and_create_fraction\n"

		  	"bin_div_arg1_frac_arg2_int:\n"
		  	"mov r10, rax\n"
		  	"CAR r10\n"
		  	"DATA r10\n"
		  	"mov r12, rax\n"
		  	"CDR r12\n"
		  	"DATA r12\n"
		  	"mov r8,rbx\n"
		  	"DATA r8\n"
		  	"MULT r8,r12\n"
		  	"cmp r8, 0\n"
		  	"jg bin_div_positive_int3\n"
		  	"neg r8\n"
		  	"neg r10\n"
		  	"bin_div_positive_int3:\n"
		  	"mov rax,r8\n";now rax holds first_denominator*second_denominator
		  	"mov r11, rax\n"
		  	"mov rax, r10\n"
		  	"mov r12, r11\n"


		  	;rax holds new numerator
		  	;r12 holds new denominator
		  	"bin_div_simplify_and_create_fraction:\n"
		  	"mov r8,  rax\n"
		  	"mov r9, r12\n"
		  	"push r9\n"
		  	"push r8\n"
		  	"call simplify_fraction\n"
		  	"add rsp, 16\n"
		  	"jmp bin_div_create_new_fraction\n"

		  	"bin_div_arg1_int_arg2_int:\n"
		  	"DATA rax\n"
		  	"DATA rbx\n"
		  	"mov r12, rbx\n"
		  	"cmp r12, 0\n"
		  	"jg bin_div_positive_int4\n"
		  	"neg r12\n"
		  	"neg rax\n"
		  	"bin_div_positive_int4:\n" 
		  	"jmp bin_div_simplify_and_create_fraction\n"
		                                
		  	"bin_div_create_new_fraction:\n"
		  	"cmp r9, 1\n"
		  	"je bin_div_create_new_int\n"

		  	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"		;r8 holds numerator in data structure
		  	"shl r9, 4\n"
		  	"or r9, T_INTEGER\n"		;r9 holds denominator in data structure
		    
		    "mov rdi, 8\n"
		    "call malloc\n"
		    "mov rbx, rax\n"
		    "mov [rbx], r8\n"
		    "push rbx\n"

		    "mov rdi, 8\n"
		    "call malloc\n"
		    "pop rbx\n"
		    "mov rcx, rax\n"
		    "mov [rcx], r9\n"
		    "push rbx\n"
		    "push rcx\n"

		    "mov rdi, 8\n"
		    "call malloc\n" 
		    "pop rcx\n"
		    "pop rbx\n"
                                               
		  	"MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx\n"
		  	"jmp bin_div_finish\n"
		          
		   	"bin_div_create_new_int:\n"

		   	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"
		  	"push r8\n"

		  	"mov rdi, 8\n"
		    "call malloc\n"
		    "pop r8\n"
		    "mov [rax], r8\n"
          
	        "bin_div_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_div_exit:\n")))

(define gen-bin-less-than
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_less_than_body\n"
            "mov [bin_less_than], rax\n"
            "jmp bin_less_than_exit\n"
            
            "bin_less_than_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne bin_less_than_finish\n"

	 		;"push const_2\n"
	 		"push An(1)\n"
	 		"push An(0)\n"
	 		"push 2\n"
	 		"push qword 0\n"
	 		"call bin_minus_body\n"
	 		"mov rax, [rax]\n"
	 		"mov rbx, rax\n"
	 		"TYPE rbx\n"
	 		"cmp rbx, T_INTEGER\n"
	 		"je check_sign\n"
	 		"CAR rax\n"

	 		"check_sign:\n"
	 		"DATA rax\n"
	 		"cmp rax, 0\n"
	 		"jl bin_less_than_true\n"
	        
	        "mov rax, const_4\n"
	        "jmp bin_less_than_finish\n"

	        "bin_less_than_true:\n"
	        "mov rax, const_3\n"

	        "bin_less_than_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_less_than_exit:\n" )))

(define gen-bin-greater-than
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_greater_than_body\n"
            "mov [bin_greater_than], rax\n"
            "jmp bin_greater_than_exit\n"
            
            "bin_greater_than_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne bin_greater_than_finish\n"

	 		;"push const_2\n"
	 		"push An(1)\n"
	 		"push An(0)\n"
	 		"push 2\n"
	 		"push qword 0\n"
	 		"call bin_minus_body\n"
	 		"mov rax, [rax]\n"
	 		"mov rbx, rax\n"
	 		"TYPE rbx\n"
	 		"cmp rbx, T_INTEGER\n"
	 		"je bin_greater_than_check_sign\n"
	 		"CAR rax\n"

	 		"bin_greater_than_check_sign:\n"
	 		"DATA rax\n"
	 		"cmp rax, 0\n"
	 		"jg bin_greater_than_true\n"
	        
	        "mov rax, const_4\n"
	        "jmp bin_greater_than_finish\n"

	        "bin_greater_than_true:\n"
	        "mov rax, const_3\n"

	        "bin_greater_than_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_greater_than_exit:\n" )))

(define gen-char->integer
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, char_to_integer_body\n"
            "mov [char_to_integer], rax\n"
            "jmp char_to_integer_exit\n"
            
            "char_to_integer_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne char_to_integer_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n"
	        "TYPE rax\n"
	        "cmp rax, T_CHAR\n"
	        "jne char_to_integer_finish\n"

	        "sub rbx, T_CHAR\n"
	        "or rbx, T_INTEGER\n"

	        "mov rdi,8\n"
	        "call malloc\n"
	        "mov qword [rax], rbx\n"

	        "char_to_integer_finish:\n"
	        "leave\n"
	        "ret\n"
	        "char_to_integer_exit:\n" )
	))

(define gen-integer->char
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, integer_to_char_body\n"
            "mov [integer_to_char], rax\n"
            "jmp integer_to_char_exit\n"
            
            "integer_to_char_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne integer_to_char_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "jne integer_to_char_finish\n"

	        "sub rbx, T_INTEGER\n"
	        "or rbx, T_CHAR\n"

	        "mov rdi,8\n"
	        "call malloc\n"
	        "mov qword [rax], rbx\n"

	        "integer_to_char_finish:\n"
	        "leave\n"
	        "ret\n"
	        "integer_to_char_exit:\n" )
	))

(define gen-numerator
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, numerator_body\n"
            "mov [numerator], rax\n"
            "jmp numerator_exit\n"
            
            "numerator_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne numerator_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "je get_integer_numerator\n"
	        "cmp rax, T_FRACTION\n"
	        "jne numerator_finish\n"
	        "mov rax, An(0)\n"
	        "mov rax, [rax]\n"

	        "DATA_UPPER rax\n"
			"add rax, start_of_data\n"
	        "jmp numerator_finish\n"

	        "get_integer_numerator:\n"
	        "mov rax, An(0)\n"

	        "numerator_finish:\n"
	        "leave\n"
	        "ret\n"
	        "numerator_exit:\n" )
	))

(define gen-denominator
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, denominator_body\n"
            "mov [denominator], rax\n"
            "jmp denominator_exit\n"
            
            "denominator_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne denominator_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "je get_integer_denominator\n"
	        "cmp rax, T_FRACTION\n"
	        "jne denominator_finish\n"
	        "mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "DATA_LOWER rax\n"
			"add rax, start_of_data\n"
	        "jmp denominator_finish\n"

	        "get_integer_denominator:\n"
	        "mov rbx, MAKE_LITERAL(T_INTEGER,1)\n"
	        "mov rdi,8\n"
	        "call malloc\n"    
	        "mov qword [rax], rbx\n"

	        "denominator_finish:\n"
	        "leave\n"
	        "ret\n"
	        "denominator_exit:\n" )
	))

(define gen-remainder
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, remainder_body\n"
            "mov [remainder], rax\n"
            "jmp remainder_exit\n"
            
            "remainder_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne remainder_finish\n"

	 		"mov rax, An(0)\n"
	 		"mov rax, [rax]\n"
	 		"mov rcx, rax\n"
	 		"mov rbx, An(1)\n"
	 		"mov rbx, [rbx]\n"
	 		"mov r10, rbx\n"
	 		"TYPE rcx\n"
	        "cmp rcx, T_INTEGER\n"
	        "jne remainder_finish\n"
	        "TYPE rbx\n"
	        "cmp rbx, T_INTEGER\n"
	        "jne remainder_finish\n"
	        "DATA rax\n"
	        "mov r9, rax\n"
	        "DATA r10\n"
	        "mov rdx, qword 0\n"
	      
	        "cmp r9, 0\n"
	        "jge is_not_negative1\n"
	        "neg rax\n"
	        "is_not_negative1:\n"
	        "mov rdx, qword 0\n"
	        "idiv r10\n"
	        "cmp r9, 0\n"
	        "jge is_not_negative2\n"
	        "neg rdx\n"
	        "is_not_negative2:\n"

	        "shl rdx, TYPE_BITS\n"
	        "add rdx, T_INTEGER\n"
	        "push rdx\n"
	        "mov rdi, 8\n"
	        "call malloc\n"
	        "pop rdx\n"
	        "mov [rax], rdx\n"
	        
	        "remainder_finish:\n"
	        "leave\n"
	        "ret\n"
	        "remainder_exit:\n" )))

(define gen-string-length
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, string_length_body\n"
            "mov [string_length], rax\n"
            "jmp string_length_exit\n"
            
            "string_length_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne string_length_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "mov rbx,rax\n"
	        "TYPE rax\n"
	        "cmp rax, T_STRING\n"
	        "jne string_length_finish\n"
	        "STRING_LENGTH rbx\n"
	        "shl rbx, TYPE_BITS\n"
	        "add rbx, T_INTEGER\n"
	        "mov rdi,8\n"
	        "call malloc\n"
	        "mov [rax], rbx\n"

	        "string_length_finish:\n"
	        "leave\n"
	        "ret\n"
	        "string_length_exit:\n" )
	))

(define gen-vector-length
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, vector_length_body\n"
            "mov [vector_length], rax\n"
            "jmp vector_length_exit\n"
            
            "vector_length_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne vector_length_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "mov rbx,rax\n"
	        "TYPE rax\n"
	        "cmp rax, T_VECTOR\n"
	        "jne vector_length_finish\n"
	        "VECTOR_LENGTH rbx\n"
	        "shl rbx, TYPE_BITS\n"
	        "add rbx, T_INTEGER\n"
	        "mov rdi,8\n"
	        "call malloc\n"
	        "mov [rax], rbx\n"

	        "vector_length_finish:\n"
	        "leave\n"
	        "ret\n"
	        "vector_length_exit:\n" )
	))

(define gen-string-ref
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, string_ref_body\n"
            "mov [string_ref], rax\n"
            "jmp string_ref_exit\n"
            
            "string_ref_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne string_ref_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n"
	        
	        "mov rcx, An(1)\n"
	        "mov rcx, [rcx]\n"
	        "mov rdx, rcx\n"
	        "DATA rdx\n"

	        "TYPE rax\n"
	        "cmp rax, T_STRING\n"
	        "jne string_ref_finish\n"

	        "TYPE rcx\n"
	        "cmp rcx, T_INTEGER\n"
	        "jne string_ref_finish\n"

	        "STRING_REF cl, rbx, rdx\n"
	        "shl rcx, TYPE_BITS\n"
	        "add rcx, T_CHAR\n"
	        "push rcx\n"
	        "mov rdi,8\n"
	        "call malloc\n"
	        "pop rcx\n"
	        "mov [rax], rcx\n"

	        "string_ref_finish:\n"
	        "leave\n"
	        "ret\n"
	        "string_ref_exit:\n" )))

(define gen-vector-ref
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, vector_ref_body\n"
            "mov [vector_ref], rax\n"
            "jmp vector_ref_exit\n"
            
            "vector_ref_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne vector_ref_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n"
	        
	        "mov rcx, An(1)\n"
	        "mov rcx, [rcx]\n"
	        "mov rdx, rcx\n"
	        "DATA rdx\n"

	        "TYPE rax\n"
	        "cmp rax, T_VECTOR\n"
	        "jne vector_ref_finish\n"

	        "TYPE rcx\n"
	        "cmp rcx, T_INTEGER\n"
	        "jne vector_ref_finish\n"

	        "VECTOR_REF rcx, rbx, rdx\n"
	        "push rcx\n"
	        "mov rdi,8\n"
	        "call malloc\n"
	        "pop rcx\n"
	        "mov [rax], rcx\n"

	        "vector_ref_finish:\n"
	        "leave\n"
	        "ret\n"
	        "vector_ref_exit:\n" )))

(define gen-string-set
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, string_set_body\n"
            "mov [string_set], rax\n"
            "jmp string_set_exit\n"
            
            "string_set_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 3\n" 
	 		"jne string_set_finish\n"

	 		"mov rax, An(0)\n" ;string	        
	        
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n" 

	        "mov r11, An(1)\n" ;index
	        "mov r11, [r11]\n"
	        "mov rdx, r11\n"
	        "DATA rdx\n"

	       	"mov r10, An(2)\n" ;char
	        "mov r10, [r10]\n"
	        "mov rcx, r10\n"
	        "DATA rcx\n"

	        "TYPE rax\n"
	        "cmp rax, T_STRING\n"
	        "jne string_set_finish\n"

	        "TYPE r11\n"
	        "cmp r11, T_INTEGER\n"
	        "jne string_set_finish\n"

	       	"TYPE r10\n"
	        "cmp r10, T_CHAR\n"
	        "jne string_set_finish\n"

	        ;rbx=string, rdx=index, rcx=char

	        "mov r12, rbx\n"
	        "STRING_ELEMENTS rbx\n"
			"add rbx, rdx\n"
			"mov byte [rbx], cl\n"

	        "mov rax, const_1\n"

	        "string_set_finish:\n"
	        "leave\n"
	        "ret\n"
	        "string_set_exit:\n" )))

(define gen-vector-set
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, vector_set_body\n"
            "mov [vector_set], rax\n"
            "jmp vector_set_exit\n"
            
            "vector_set_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 3\n" 
	 		"jne vector_set_finish\n"

	 		"mov rax, An(0)\n" ;vector	        
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n" 

	        "mov r11, An(1)\n" ;index
	        "mov r11, [r11]\n"
	        "mov rdx, r11\n"
	        "DATA rdx\n"

	       	"mov rcx, An(2)\n" ;address of item

	        "TYPE rax\n"
	        "cmp rax, T_VECTOR\n"
	        "jne vector_set_finish\n"

	        "TYPE r11\n"
	        "cmp r11, T_INTEGER\n"
	        "jne vector_set_finish\n"

	        ;rbx=vector, rdx=index, rcx=item

	        "mov r12, rbx\n"
	        "VECTOR_ELEMENTS r12\n"
	        "mov [r12 + rdx*8], rcx\n"

	        "mov rax, const_1\n"

	        "vector_set_finish:\n"
	        "leave\n"
	        "ret\n"
	        "vector_set_exit:\n" )))

(define gen-make-string
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, make_string_body\n"
            "mov [make_string], rax\n"
            "jmp make_string_exit\n"
            
            "make_string_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"
			"mov rdx, qword 0\n" ;initialize char with 0         
            "mov r9, arg_count\n"
	        "cmp r9, 2\n"
	 		"jg make_string_finish\n"

	 		"mov rax, An(0)\n" ;length of string	        
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n"
	        "DATA rbx\n" 

	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "jne make_string_finish\n"

	        "cmp r9, 1\n"
	        "je start_creating_string\n"

	        "mov rcx, An(1)\n" ;char
	        "mov rcx, [rcx]\n"
	        "mov rdx, rcx\n"
	        "DATA rdx\n"

	        "TYPE rcx\n"
	        "cmp rcx, T_CHAR\n"
	        "jne make_string_finish\n"

	        "start_creating_string:\n"

	        "push rbx\n"
	        "push rdx\n"
	        "mov rdi, rbx\n"
	        "call malloc\n"
	        "pop rdx\n"
	        "pop rbx\n"

 
	        ;rax= pointer to address of rbx bytes, rbx=length of string, rdx=char
	        "mov r10, 0\n" ;counter

	        "for_create_string:\n"
	        "cmp r10, rbx\n"
	        "je end_of_create_string\n"
	        "mov byte [rax+r10], dl\n"
	        "inc r10\n"
	        "jmp for_create_string\n"
	        "end_of_create_string:\n"

	        "mov rcx, rax\n"

	        "MAKE_LITERAL_STRING_WITH_REGS rcx, rbx\n" ;at the end of macro, rax is a literal string
	        "mov rcx, rax\n"
	        "push rcx\n"
	        "mov rdi, 8\n"
	        "call malloc\n"
	        "pop rcx\n"

	        "mov [rax], rcx\n"

	        "make_string_finish:\n"
	        "leave\n"
	        "ret\n"
	        "make_string_exit:\n" )))

(define gen-make-vector
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, make_vector_body\n"
            "mov [make_vector], rax\n"
            "jmp make_vector_exit\n"
            
            "make_vector_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"  
			;initialize item with 0
			"mov rdi, 8\n"
			"call malloc\n"
			"mov rdx, 0\n"
			"shl rdx, TYPE_BITS\n"
	        "add rdx, T_INTEGER\n"       
            "mov rbx, arg_count\n"
            "mov [rax], rdx\n" 
            "mov rdx, rax\n" ;now rdx conatins integer 0
	        "cmp rbx, 2\n" 
	 		"jg make_vector_finish\n"

	 		"mov rax, An(0)\n" ;length of vector	        
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n"
	        "DATA rbx\n" 

	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "jne make_vector_finish\n"

	        "mov r9, arg_count\n"
	        "cmp r9, 1\n"
	        "je start_creating_vector\n"
	        "mov rdx, An(1)\n" ; address of item

	        "start_creating_vector:\n"

	        "push rbx\n"
	        "push rdx\n"
	        "shl rbx, 3\n"
	        "mov rdi, rbx\n"
	        "call malloc\n"
	        "pop rdx\n"
	        "pop rbx\n"
 
	        ;rax= pointer to address of rbx*8 bytes, rbx=length of vector, rdx=address of item
	        "mov r10, 0\n" ;counter

	        "for_create_vector:\n"
	        "cmp r10, rbx\n"
	        "je end_of_create_vector\n"
	        "mov qword [rax+r10*8], rdx\n"
	        "inc r10\n"
	        "jmp for_create_vector\n"
	        "end_of_create_vector:\n"

	        "mov rcx, rax\n"
	        "shl rbx, 3\n"
	        "MAKE_LITERAL_VECTOR_WITH_REGS rcx, rbx\n" ;at the end of macro, rax is a literal vector
	        "mov rcx, rax\n"
	        "push rcx\n"
	        "mov rdi, 8\n"
	        "call malloc\n"
	        "pop rcx\n"
	        "mov [rax], rcx\n"

	        "make_vector_finish:\n"
	        "leave\n"
	        "ret\n"
	        "make_vector_exit:\n" )))

(define gen-vector
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, custom_vector_body\n"
            "mov [custom_vector], rax\n"
            "jmp custom_vector_exit\n"
            
            "custom_vector_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"

	        "push rbx\n"
	        "shl rbx, 3\n"
	        "mov rdi, rbx\n"
	        "call malloc\n"
	        "pop rbx\n"
 
	        ;rax= pointer to address of rbx*8 bytes, rbx=length of vector
	        "mov r10, 0\n" ;counter
	        "for_vector:\n"
	        "cmp r10, rbx\n"
	        "je end_of_vector\n"

	        "mov rdx, An(r10)\n" 
	        "mov qword [rax+r10*8], rdx\n"
	        "inc r10\n"
	        "jmp for_vector\n"
	        "end_of_vector:\n"

	        "mov rcx, rax\n"
	        "shl rbx, 3\n"
	        "MAKE_LITERAL_VECTOR_WITH_REGS rcx, rbx\n" ;at the end of macro, rax is a literal vector
	        "mov rcx, rax\n"
	        "push rcx\n"
	        "mov rdi, 8\n"
	        "call malloc\n"
	        "pop rcx\n"
	        "mov [rax], rcx\n"

	        "custom_vector_finish:\n"
	        "leave\n"
	        "ret\n"
	        "custom_vector_exit:\n" )))

(define gen-apply
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, apply_body\n"
            "mov [apply], rax\n"
            "jmp apply_exit\n"
            
            "apply_body:\n"
    		"push rbp\n"
            "mov rbp, rsp\n"
	        "mov rax, An(0)\n"				;closure
	        "mov rax, qword [rax]\n"
	        "mov r10, qword [rbp]\n" 		;old rbp
	        "mov r11,qword [rbp+8]\n" 		;ret addr
	        "mov r12, rbp\n"
	        "add r12, 5*8\n"
	        
	        "mov rbx, rax\n" 
	        "TYPE rbx\n"
	        "cmp rbx, T_CLOSURE\n"
	        "jne apply_finish\n"
	         
	        "mov rcx, An(1)\n"
	        "mov rcx, qword [rcx]\n"
	        "mov rbx, rcx\n"
	        "TYPE rbx\n"
	        "cmp rbx, T_PAIR\n"
	        "je apply_start\n"

	        "cmp rbx, T_NIL\n"
	        "jne apply_finish\n"

	        "apply_start:\n"
	        "mov rsi, 0\n"

		    "apply_calculate_list_length:\n"
		    "cmp rbx, T_NIL\n"
		    "je apply_calculate_list_length_done\n"
		    "CDR rcx\n"
		    "mov rbx, rcx\n"
		    "TYPE rbx\n"
		    "inc rsi\n"
		    "jmp apply_calculate_list_length\n"

	        "apply_calculate_list_length_done:\n"
	        "shl rsi, 3\n"
	        "sub r12, rsi\n"
	        "shr rsi, 3\n"
	        
	        "mov rdi, 0\n"
	        "mov rcx, An(1)\n"  			 
	        "mov rcx, qword [rcx]\n"

	        "apply_loop:\n"

	        "cmp rdi, rsi\n"
	        "je apply_loop_exit\n"
	        "mov rbx, rcx\n"
	        "DATA_UPPER rbx\n"
			"add rbx, start_of_data\n"    
	        "mov qword [r12 + 8*rdi], rbx\n"
	        "CDR rcx\n"
	        "inc rdi\n"
	        "jmp apply_loop\n"
	        
	        "apply_loop_exit:\n"

	        "sub r12, 8\n"
	        "mov qword [r12],rsi\n"
	        "sub r12, 8\n"
	        "mov rbx, rax\n"
	        "CLOSURE_ENV rbx\n"
	        "mov qword [r12], rbx\n"
	        "sub r12, 8\n"
	        "mov qword [r12], r11\n"
	        "mov rsp, r12\n"	        
	        "mov rbp, r10\n"
	        "mov rbx, rax\n"
	        "TYPE rbx\n"
	        
	        "cmp rbx, T_CLOSURE\n"
	        "jne apply_finish\n"
	        "CLOSURE_CODE rax\n"
	        "jmp rax\n"
	        "apply_finish:\n"
	        "leave\n"
	        "ret\n"
	        "apply_exit:\n" )))


(define gen-symbol->string
	(lambda ()
		(string-append
			"mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, symbol_to_string_body\n"
            "mov [symbol_to_string], rax\n"
            "jmp symbol_to_string_exit\n"
            
            "symbol_to_string_body:\n"
    		"push rbp\n"
            "mov rbp, rsp\n"
            "mov rax, An(0)\n"
            "mov rax, [rax]\n"
            "DATA rax\n"
			"add rax , start_of_data\n"
	        "symbol_to_string_finish:\n"
	        "leave\n"
	        "ret\n"
	        "symbol_to_string_exit:\n" )))

(define gen-string->symbol
	(lambda ()
		(string-append
			"mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, string_to_symbol_body\n"
            "mov qword [string_to_symbol], rax\n"
            "jmp string_to_symbol_exit\n"
            
            "string_to_symbol_body:\n"
			"push rbp\n"
			"mov rbp, rsp\n"
			"mov r11, An(0)\n" ;r11= pointer to arg
			;"mov r11, [r11]\n"
		    "mov r10, [symbol_table]\n" ;content of fymbol_table, either a pair or const_2
		    ;"mov r10, [r10]\n"				
	        "cmp r10, const_2\n" 
	        "je string_to_symbol_create_symbol\n"
    		
	        "string_to_symbol_loop:\n"
			"mov r12, r10\n"
			"mov r12, [r12]\n"
			"DATA_UPPER r12\n"
			"add r12 , start_of_data\n"
			"mov r12, [r12]\n"
			"DATA r12\n"
			"add r12 , start_of_data\n"
      		;"cmp r12, r11\n"  
      		"STRING_COMPARE r12, r11\n"
      		"cmp rax, const_3\n" 
	        "je string_to_symbol_found\n"
		    ;"CDR r10\n"
		    "mov r10, [r10]\n"
		    "DATA_LOWER r10\n"
			"add r10, start_of_data\n"
			"cmp r10, const_2\n"
      		"je string_to_symbol_create_symbol\n"
	        "jmp string_to_symbol_loop\n"
	            
	        "string_to_symbol_found:\n"
	        "mov r10, [r10]\n"
	        "DATA_UPPER r10\n"
			"add r10, start_of_data\n"
		   	"mov rax, r10\n"
	        "jmp string_to_symbol_finish\n"
	      
	        "string_to_symbol_create_symbol:\n"
	        "push r11\n"
	        "mov rdi,8\n"
	        "call malloc\n"
	        "pop r11\n"
	        "MAKE_MALLOC_LITERAL_SYMBOL rax , r11\n"
			"mov r11, rax\n"
      		"mov r13, r11\n"                    ;backup
      		"mov r14, [symbol_table]\n"       
      
      		"push r11\n"
      		"push r14\n"
       		"mov rdi, 8\n"
       		"call malloc\n"
       		"pop r14\n"
       		"pop r11\n"
       		"MAKE_MALLOC_LITERAL_PAIR rax, r11 ,r14\n"
      		"mov [symbol_table],rax\n"
      		"mov rax, r13\n"

	        "string_to_symbol_finish:\n"
	        "leave\n"
	        "ret\n"
	        "string_to_symbol_exit:\n"      
	)))

(define compile-scheme-file
	(lambda (source-file target-file)
		(let* ((parsed-exp-list 	(pipeline (append-scheme-lib-functions (file->list source-file))))
			   (epilogue 			"push qword [rax]\ncall write_sob_if_not_void\nadd rsp, 1*8\n"))
			   (create-const-table parsed-exp-list)
			   (create-global-var-table parsed-exp-list)
			   (string->file
					target-file
					(string-append
						"%include \"project/scheme.s\"\nsection .data\nstart_of_data:\n"
						(get-const-string)
						(get-fvar-string)
						(gen-symbol-table const-table 0 (count-symbols))
						"\nsection .text\nmain:\n"
						"push 0\n"
						"push 0\n"
						"push exit_compilation\n"
						"push rbp\n"
						"mov rbp, rsp\n"
						(code-gen-library-functions)
						(fold-left (lambda(acc pe) 
	                                    (string-append acc (code-gen pe) epilogue))
									""
								parsed-exp-list)
						"exit_compilation:\n"
						"leave\n"
						"mov rax, 0\n"
						"call exit\n"
						))))) 

;(let ((input-filename (cadr (command-line)))
;	 (output-filename (caddr (command-line))))
;	 (compile-scheme-file input-filename output-filename))