(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")

(define first
  (lambda (lst)
    (car lst)))

(define second
  (lambda (lst)
    (cadr lst)))

(define third
  (lambda (lst)
    (caddr lst)))

(define forth
  (lambda (lst)
    (cadddr lst)))

(define file->list
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
		(lambda ()
		  (let ((readChar (read-char in-port)))
		    (if (eof-object? readChar)
			(begin
			  (close-input-port in-port)
			  '())
			(cons readChar (run)))))))
	(run)))))

(define file->string
  (lambda (in-file)
    (list->string (file->list in-file))))

(define list->file
  (lambda (lst out-file)
    (let ((out-port (open-output-file out-file 'truncate)))
      (letrec ((run
                (lambda (lst)
                  (if (null? lst)
                      (close-output-port out-port)
                      (begin (write-char (car lst) out-port)
                             (run (cdr lst)))))))
        (run lst)))))


(define pipeline
  (lambda (sexpr)
    ((star <sexpr>) sexpr
     (lambda (match rest)
       (map (lambda (expr)
	      (annotate-tc
	       (pe->lex-pe
		(box-set
		 (remove-applic-lambda-nil
		  (parse expr))))))
	    match))
     (lambda (fail) 'fail))))



(define list->sexprs
  (lambda (lst)
    ;;(display (format "list->sexprs[lst] = ~a\n"lst))
        (pipeline lst)))

(define string->sexprs
  (lambda (str)
    (let ((stringList (string->list str)))
      (letrec ((translate
                (lambda (lst)
                  (<sexpr> lst 
                           (lambda (expr rest) (if (null? rest)
                                             (list expr)
                                             (cons expr (translate rest))))
                           (lambda (msg) `(error ,@msg))))))
        (translate stringList)))))

(define create-code-to-run
  (lambda (sexprs)
    ;;(display (format "Sexprs: ~a\n" sexprs))
    (fold-left string-append
	       ""
	       (map (lambda (expr)
		      (string-append
		       (code-gen expr)
		       cg-print-rax))
		    sexprs))))

(define compile-scheme-file
  (lambda (source dest)
    (let* ((pipelined (list->sexprs (file->list source)))
	   (size (length pipelined)))
      ;;(display (format "Before c-table...\n"))
      (set! c-table (master-build-c-table pipelined 6))
      ;;(display (format "C-Table:\n~a\n" c-table))
	  (set! f-table (master-build-f-table pipelined))
      ;;(display (format "F-Table:\n~a\n" f-table))
      ;;(display (format "pipelinded = ~a\n" pipelined))
      (let* ((pre (generate-pre-text c-table f-table))
	    (code (create-code-to-run pipelined)))
	;;(display (format "Pre-Text:\n~a\nCode:\n~b\n" pre code))
	(list->file (string->list (string-append pre
						 newLine
						 code
						 post-text))
		  dest)
      (display (format "Compiled Scheme file with ~a parsed expressions!\n" size))))))

;--------------------------------------------------| cTable |--------------------------------------------------------

(define T_UNDEFINED 0)
(define T_VOID 1)
(define T_NIL 2)
(define T_INTEGER 3)
(define T_FRACTION 4)
(define T_BOOL 5)
(define T_CHAR 6)
(define T_STRING 7)
(define T_SYMBOL 8)
(define T_CLOSURE 9)
(define T_PAIR 10)
(define T_VECTOR 11)


;  is empty or not a list   -> returns saved results
;  car passes test          -> save car, remove it and do on the rest
;  car is a list            -> open the car and do again
;  else                     -> remove car and do on the rest
(define those-that-pass
  (lambda (exps test positive-results)
    (cond 
     ((or (not (pair? exps))
	  (null? exps))
      positive-results)
     
     ((test (car exps))
      (those-that-pass (cdr exps) test (cons (car exps) positive-results)))
     
     ((pair? (car exps)) (those-that-pass `(,@(car exps) ,@(cdr exps)) test positive-results))
     
     (else (those-that-pass (cdr exps) test positive-results)))))

					; returns deep search, returns elements that pass test
					; TODO: if exps passes test, do not go into the those-that-pass function
(define ordered-those-that-pass
  (lambda (exps test)
    (reverse (those-that-pass exps test '()))))

(define tagged-by-const
  (lambda (exp)
    (and (pair? exp) (equal? (car exp) 'const))))

(define extract-consts
  (lambda (exp)
    (if (tagged-by-const exp)
	(cdr exp)
	(map (lambda (x) (cadr x))
	     (ordered-those-that-pass exp tagged-by-const)))))

(define extract-and-topo-sort-consts
  (lambda (exp done)
    (if (null? exp) 
      done
      (extract-and-topo-sort-consts (cdr exp) (append done (reverse (topological-sort (car exp))))
				    ))))

(define master-const-extract 
  (lambda (exp)
    (if (null? exp)
	'()
	(extract-and-topo-sort-consts (extract-consts exp) '()))))

(define float->integer-func ;not very efficient, #f if didnt find or fnum being integer
	(lambda (fnum guess)
		(cond ((equal? 0.0 (- fnum guess)) guess)
			  ((> guess fnum) #f)
			  (else (float->integer-func fnum (+ 1 guess)))))) 
(define float->integer ;not very efficient, #f if didnt find or fnum being integer
	(lambda (fnum)
		(float->integer-func fnum 0)))

(define add-to-c-table  ;returns (table . nextmem)
  (lambda (table element mem)
    (cond ((char? element)
	   ;; T_Char <index, value, (T_Char, value)>
	   (cons (append table
			 `((,mem ,element 
				 (,T_CHAR ,(char->integer element)))))
		 (+ 2 mem) ))
          ((integer? element)
	   ;; <index, value, (T_Integer, value)>
	   (cons (append table
			 `((,mem ,element
				 (,T_INTEGER ,element))))
		 (+ 2 mem)))
          ((rational? element)
	   ;; <index, value, (T_Fraction, num, denum)>
	   (let* ((top (numerator element))
		  (bottom (denominator element))
		  (topIndx (c-table-contains? table top))
		  (bottomIndx (c-table-contains? table bottom)))
	     ;;(display (format "Adding T_FRACTION to C-table: ~a Rational? ~a\n" element (rational? element)))
	     ;;(display (format "Num:~a\nDenum:~a\n" top bottom))
	     ;;(display (format "Num Index:~a\nDenum Index:~a\n" topIndx bottomIndx))
             (cond ((and topIndx bottomIndx)
		    ;;has both ints -> add fraction          
		    (cons (append table `((,mem ,element
						(,T_FRACTION ,topIndx ,bottomIndx))))
			  (+ 3 mem)))
		   (topIndx										 ;;has only numerator -> add denominator and do again
		    (add-to-c-table (car (add-to-c-table table bottom mem))
				    element
				    (cdr (add-to-c-table table bottom mem))))
		   (else
		    ;;has only maybe the denominator -> add numerator and do again 
		    (add-to-c-table (car (add-to-c-table table top mem))
				    element
				    (cdr (add-to-c-table table top mem)))))))
          ((string? element)
	   ;; <index, value, (T_STRING, length, ASCII-list)>
	   (cons (append table
			 `((,mem ,element
				 (,T_STRING ,(string-length element) ,(map char->integer (string->list element))))))
		 (+ mem (string-length element) 2))) 
          ((symbol? element)
	   ;; <index, symbol, (T_Symbol, string)>
           (let ((rep-str (symbol->string element)))
             (if (c-table-contains? table rep-str)
                 (cons (append table
			       `((,mem ,element (,T_SYMBOL ,(c-table-contains? table rep-str)))))
		       (+ 2 mem))
                 (add-to-c-table (car (add-to-c-table table rep-str mem))
				 element
				 (cdr (add-to-c-table table rep-str mem))))))
          ((pair? element)
	   ;; <index, value, (T_PAIR, car-index, cdr-index)>
	   (let ((carIndex (c-table-contains? table (car element)))
		 (cdrIndex (c-table-contains? table (cdr element))))
	     (cond ((and carIndex cdrIndex)
		    (cons (append table
				  `((,mem ,element (,T_PAIR ,carIndex ,cdrIndex))))
			  (+ 3 mem)))
		   (carIndex
		    (add-to-c-table (car (add-to-c-table table (second element) mem))
				    element
				    (cdr (add-to-c-table table (second element) mem))))
		   (else (add-to-c-table (first (add-to-c-table table (first element) mem))
					 element
					 (second (add-to-c-table table (first element) mem)))))))
	  
          ((vector? element)
	   ;; <index, value, (T_Vector, length, index-list-of-elements)>
	   (cons (append table
			 `((,mem ,element (,T_VECTOR
					   ,(vector-length element)
					   ,(map (lambda (x)
						   (c-table-contains? table x))
						 (vector->list element))))))
		 (+ mem (vector-length element) 2)))
          (else 'error))))

(define last-mem
  (lambda (table starting-mem)
    (if (null? table) starting-mem (caar (last-pair table)))))

(define c-table-contains? ;returns adress
  (lambda (table element)
    (cond ((null? table) #f)
          ((equal? element (cadar table)) (caar table))
          (else (c-table-contains? (cdr table) element)))))

(define build-c-table-func
  (lambda (table lst mem)
    (cond  ((null? lst)
	    table)
           ((c-table-contains? table (car lst))
	    (build-c-table-func table (cdr lst) mem))
           (else
	    (let* ((new-table (car (add-to-c-table table (car lst) mem)))
		   (new-mem (cdr (add-to-c-table table (car lst) mem))))
	      (build-c-table-func new-table (cdr lst) new-mem))))))


(define starting-table
  (lambda (mem)
    `((,mem ,(if #f #f) (,T_VOID)) (,(+ 1 mem) () (,T_NIL)) (,(+ mem 2) #f (,T_BOOL 0)) (,(+ mem 4) #t (,T_BOOL 1)))))


(define build-c-table
  (lambda (lst starting-mem)
    (build-c-table-func (starting-table (- starting-mem 6)) lst starting-mem)))


(define topological-sort 
  (lambda (e) 
    (cond 
     ((or (number? e) (string? e) (eq? e (if #f #f)) (null? e) (boolean? e) (char? e) ) `(,e)) 
      ((pair? e) 
       `(,e ,@(topological-sort (car e)) ,@(topological-sort (cdr e))))
      ((vector? e) 
       `(,e ,@(apply append 
                     (map topological-sort (vector->list e)))))
      ((symbol? e)
       `(,e ,@(topological-sort (symbol->string e))))
      (else 'topological-sort-error))))

(define master-build-c-table
  (lambda (exp mem)
    (build-c-table (master-const-extract exp) mem)))

(define c-table-getLine
  (lambda (table element)
      (cond ((null? table)
	     #f)
	    ((equal? element (second (first table)))
	     (first table))
	    (else
	     (c-table-getLine (cdr table) element)))))

(define c-table-getLine-byType
  (lambda (table element type)
      (cond ((null? table)
	     #f)
	    ((and (equal? element (second (first table)))
		  (equal? type (first (third table))))
	     (first table))
	    (else
	     (c-table-getLine (cdr table) element type)))))

;;a.k.a:  c-table[i] =
(define c-table '())
(define const-label "L_const")
(define fvar-label "L_global")

(define CHAR_NUL 0)
(define CHAR_TAB 9)
(define CHAR_NEWLINE 10)
(define CHAR_PAGE 12)
(define CHAR_RETURN 13)
(define CHAR_SPACE 32)

(define cg-c-table
  (lambda (ct)
    ;;(display (format "Generating C-Table...\n~a\n" ct))
    (fold-left string-append
	       (list->string '())
	       (map (lambda (row)
		      ;; Row = <Index, Value, (Type, Type-Data)>
		      (let* ((index (first row))
			     (value (second row))
			     (data (third row))
			     (type (first data))
			     (type-data (cdr data)))
			(cond
			 ((equal? T_VOID type)
			  (cg-T-void index))
			 ((equal? T_NIL type)
			  (cg-T-nil index))
			 ((equal? T_INTEGER type)
			  (cg-T-integer value index))
			 ((equal? T_FRACTION type)
			  (cg-T-fraction (first type-data) (second type-data) index))
			 ((equal? T_BOOL type)
			  (cg-T-bool (first type-data) index))
			 ((equal? T_CHAR type)
			  (cg-T-char value index))
			 ((equal? T_STRING type)
			  (cg-T-string (second data) (third data) index))
			 ((equal? T_SYMBOL type)
			  (cg-T-symbol (first type-data) index))
			 ((equal? T_PAIR type)
			  (cg-T-pair (first type-data) (second type-data) index))
			 ((equal? T_VECTOR type)
			  (cg-T-vector (first type-data) (second type-data) index))
			 (else (number->string T_UNDEFINED)))))
		    ct)
	       )))

(define make-const-label
  (lambda (index)
    (string-append const-label (number->string index) ":" newLine)))

(define make-fvar-label
  (lambda (index)
    (string-append fvar-label (number->string index) ":" newLine)))

(define cg-T-void
  (lambda (index)
    (string-append (make-const-label index)
		   tab "dq SOB_VOID" newLine)))

(define cg-T-nil
  (lambda (index)
    (string-append (make-const-label index)
		   tab "dq SOB_NIL" newLine)))

(define cg-T-bool
  (lambda (value index)
    (let ((true (equal? value 1)))
      (string-append (make-const-label index)
		     tab (if true
			     "dq SOB_TRUE" 
			     "dq SOB_FALSE")
		     newLine))))

(define get-T-char-value
  (lambda (value)
    (let ((val (char->integer value)))
      ;;(display (format "Search char value: ~a\nintValue = ~a\n" value val))
      (cond ((equal? val (char->integer #\newline))
	     "CHAR_NEWLINE")
	    ((equal? val (char->integer #\"))
	     "\'\"\'")
	    ((equal? val (char->integer #\\))
	     "\'\\\'")
	    ((equal? val (char->integer #\tab))
	     "\'\t\'")
	    (else (string-append "\'" (string value) "\'"))))))

(define cg-T-char
  (lambda (value index)
    ;;(display (format "c-gen to T_CHAR: ~a\n" value))
    (string-append (make-const-label index)
		   tab "dq MAKE_LITERAL(T_CHAR, " (get-T-char-value value) ")" newLine)))

(define cg-T-integer
  (lambda (value index)
    (string-append (make-const-label index)
		   tab "dq MAKE_LITERAL(T_INTEGER, " (number->string value) ")" newLine)))

(define cg-T-fraction
  (lambda (numIndx denumIndx index)
    (string-append (make-const-label index)
		   tab "dq MAKE_LITERAL_FRACTION(" const-label (number->string numIndx) ", " const-label (number->string denumIndx) ")" newLine)))

(define append-params
  (lambda (params)
    (fold-left (lambda (result current)
		 (string-append result
				(if (not (equal? current (first params)))
				    ", "
				    " ")
				(number->string current)))
	       ""
	       params)))

(define cg-T-string
  (lambda (length chars index)
    ;;(display (format "generating string const = ~a\n" chars))
    (string-append
     (make-const-label index)
     tab "MAKE_LITERAL_STRING" (append-params chars) newLine)))

(define cg-T-symbol
  (lambda (stringIndex index)
      (string-append (make-const-label index)
		     tab "MAKE_LITERAL_SYMBOL " (string-append
						const-label
						(number->string stringIndex))
		     newLine)))

(define cg-T-pair
  (lambda (carIndex cdrIndex index)
    ;;(display (format "Generating Pair: carIndx: ~a cdrIndx: ~a\n" carIndex cdrIndex))
    (string-append (make-const-label index)
		   tab "dq MAKE_LITERAL_PAIR(" const-label (number->string carIndex) ", " const-label (number->string cdrIndex) ")" newLine)))

(define cg-T-vector
  (lambda (length items index)
    (string-append
     (make-const-label index)
     tab "MAKE_LITERAL_VECTOR " (append-params items) newLine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  F-Table  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;row = <Var-name, Index>
;(define f-table '())

;(define f-table-contains?
 ; (lambda (var ft)
 ;   (let ((row (first ft)))
 ;     (cond ((null? ft)
;	     #f)
;	    ((equal? var (first row))
;	     (second row))
;	    (else (f-table-contains? var (cdr ft)))))))

;; Returns a list of all fvar-values
;(define extract-fvars
  ;(lambda (pe)
 ;   (cond ((not (pair? pe))
;	   '())
;	  ((tag? 'fvar pe)
;	   (cdr pe))
;	  (else
;	   `(,@(extract-fvars (first pe)) ,@(extract-fvars (cdr pe)))))))

;(define build-f-table
;  (lambda (pe ft index)
;    (add-to-f-table (remove-duplicates (extract-fvars pe)) ft index)))

;(define add-to-f-table
;  (lambda (vars ft index)
 ;     (cond ((null? vars)
;	     ft)
;	    ((f-table-contains? (first vars) ft)
;	     (add-to-f-table (cdr vars) ft index))
;	    (else
;	     (add-to-f-table (cdr vars)
;			     `(,@ft `(,(first vars) ,index))
;			     (+ index 1))))))

(define f-table-get-func ;; index
	(lambda (table element)
		(cond ((null? table) #f) 
			  ((equal? (second (car table)) element) (first (car table)))
			  (else (f-table-get-func (cdr table) element)))))

(define f-table-get ;; index
	(lambda (element)
		(f-table-get-func f-table element)))
		

(define tagged-by-fvar
  (lambda (exp)
    (and (pair? exp) (equal? (car exp) 'fvar))))

(define extract-fvars
  (lambda (exp)
    (if (tagged-by-fvar exp)
  (cdr exp)
  (map (lambda (x) (cadr x))
       (ordered-those-that-pass exp tagged-by-fvar)))))

(define f-table-contains?;input is list of fvars, not the final table
  (lambda (table element)
    (cond ((null? table) #f)
          ((equal? (car table) element) element)
          (else (f-table-contains? (cdr table) element)))))

(define f-table-add 
  (lambda (table element)
    (if (f-table-contains? table element) table (append table (list element)))))

(define build-f-table
  (lambda (table lst)
    (if (null? lst) table (build-f-table (f-table-add table (car lst)) (cdr lst)))))

(define give-indxes
  (lambda (after before indx)
    (if (null? before) after (give-indxes (cons (list indx (car before)) after) (cdr before) (+ 1 indx)))))

(define master-give-indxes ;needs a list - not a single element
  (lambda (lst)
    (reverse (give-indxes '() lst 0))))

(define master-build-f-table
  (lambda (exp)
    (master-give-indxes (build-f-table '() (extract-fvars exp)))))

(define cg-f-table
  (lambda (table)
    (fold-left string-append
               (map (lambda (line)
		      (string-append
		       fvar-label (number->string (first line)) ":" newLine ;TODO: change this line to the labels
		       tab "dq MAKE_LITERAL(T_UNDEFINED, 0)" newLine
		       
		       ))
                    table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Code Generation  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tag?
  (lambda (tag pe)
    (and (list? pe)
	 (equal? tag (first pe)))))

(define code-gen
  (lambda (pe)
    ;; After each generation, the value of the generated code is in RAX
    ;; Returns string
    ;;(display (format "Code Gen to ~a\n" pe))
    (string-append ;;";" (format "~a\n" pe)
     newLine
     (cond ((tag? 'const pe)
	    (cg-const (second pe)))
	   
	   ((tag? 'pvar pe)
	    (cg-pvar pe))
	   
	   ((tag? 'bvar pe)
	    ;;(bvar x major minor)
	    (cg-bvar (third pe) (forth pe)))
	   
	   ((tag? 'fvar pe)
	    (cg-fvar (second pe)))
	   
	   ((tag? 'if3 pe)
	    ;;(if3 test dit dif)
	    (let ((test (second pe))
		  (dit (third pe))
		  (dif (forth pe)))
	      (cg-if3 test dit dif)))
	   
	   ((tag? 'or pe)
	    (cg-or (second pe) (make-label "L_orEnd")))
	   
	   ((tag? 'seq pe)
	    ;;(seq (E1 .. En))
	    (cg-seq (second pe)))
	   
	   ((tag? 'lambda-simple pe)
	    "")
	   
	   ((tag? 'lambda-opt pe)
	    "")
	   
	   ((tag? 'define pe)
	    ;; (define var value)
	    (cg-define (second pe) (third pe)))
	   
	   ((tag? 'applic pe)
	    (string-append ";" (format "~a" pe)))
	   
	   ((tag? 'tc-applic pe)
	    (string-append ";" (format "~a" pe)))
	   
	   ((tag? 'set pe)
	    ;;(set! (*var var * *) value)
	    (let* ((var (second pe))
		   (value (third pe))
		   (cg-val (code-gen value)))
	      (string-append cg-val
			     (cond ((tag? 'bvar var)
				    (cg-set-bvar (cdr var)))
				   ((tag? 'pvar var)
				    (cg-set-pvar (cdr var)))
				   ((tag? 'fvar var)
				    (cg-set-fvar (cdr var)))
				   (else "Undefined variable type"))
			     tab "MOV RAX, " sobVoid newLine)))
	   
	   ((tag? 'box pe)
	    "")
	   
	   ((tag? 'box-get pe)
	    "")
	   
	   ((tag? 'box-set? pe)
	    "")
	   
	   (else 'Code-Generation-Error!)))))

(define newLine
  (list->string '(#\newline)))

(define tab
  (list->string '(#\tab)))

(define labelIndex 0)

(define make-label
  (lambda (name)
    (set! labelIndex (+ labelIndex 1))
    (string-append name (number->string labelIndex))))

(define cg-print-rax
  (string-append
   tab "PUSH qword [RAX]" newLine
   tab "call write_sob_if_not_void" newLine
   tab "ADD rsp, 1*8" newLine))

(define cg-print-symbol
   (string-append
   tab "PUSH RAX" newLine
   tab "call write_sob_symbol" newLine
   tab "ADD rsp, 1*8" newLine))

(define cg-const
  (lambda (const)
    (let* ((row (c-table-getLine c-table const))
	   (index (first row))
	   (type (first (third row))))
      (string-append ;;".t_" const-label (number->string index) ":" newLine
		     tab "MOV RAX, " const-label (number->string index) newLine
		     ;;newLine
		     ;;cg-print-rax)
		     ))))


(define cg-or
  (lambda (lst end-label)
    ;;(display (format "sobFalse = ~a\n" (sobFalse)))
      (cond ((null? lst)
	     (list->string '()))
	    ((null? (cdr lst))
	     (let ((cg-N (code-gen (first lst))))
	       (string-append cg-N newLine
			      end-label ":" newLine)))
	    (else
	     (let ((cg-i (code-gen (first lst))))
	       (string-append cg-i newLine
			      tab "CMP RAX, " (sobFalse)
			      newLine
			      tab "JNE " end-label newLine
			      (cg-or (cdr lst) end-label)))))))

(define cg-pvar
  (lambda (pe)
    (let ((minor (third pe)))
      (string-append
       tab "MOV RAX, qword [rbp + " (number->string (+ minor 4)) "*8]" newLine))))

(define cg-bvar
  (lambda (major minor)
    (string-append
     tab "MOV RAX, qword [rbp + 2*8]" newLine
     tab "MOV RAX, qword [RAX + " major "*8]" newLine
     tab "MOV RAX, qword [RAX + " minor "*8]" newLine)))

(define cg-fvar ;var needs to be the symbol
  (lambda (var)
    (let ((undefined 0)
	  (u-label "L_error_undefined_fvar"))
    (string-append
   ;  tab "MOV RAX, [" (number->string (f-table-get var)) "]" newLine
   ;  tab "CMP RAX, " undefined newLine
   ;  tab "JE "u-label newLine
    tab "MOV RAX, " fvar-label (number->string (f-table-get var))  newLine
   ))))
(define sobFalse
  (lambda ()
    (string-append const-label (number->string (c-table-contains? c-table #f)))))

(define sobVoid (string-append const-label "0"))

(define cg-if3
  (lambda (test dit dif)
    (let ((test-cg (code-gen test))
	  (dit-cg (code-gen dit))
	  (dif-cg (code-gen dif))
	  (l-dif (make-label "L_ifDif"))
	  (l-end (make-label "L_ifEnd")))
      
      (string-append test-cg newLine
		     tab "MOV RBX, " (sobFalse) newLine
		     tab "CMP RAX, RBX" newLine
		     tab "JE " l-dif newLine
		     dit-cg newLine
		     tab "JMP " l-end newLine
		     l-dif ":" newLine
		     dif-cg newLine
		     l-end ":" newLine
		     ))))
    
(define cg-seq
  (lambda (pe)
    (fold-left (lambda (result e)
    ;;(display (format "cg-seq: e = ~a\nresult = ~b\n" e result))
		 (string-append result (code-gen e) newLine))
	       ""
	       pe)))

(define cg-set-bvar
  (lambda (var major minor)
    (string-append
     tab "MOV RBX, qword [rbp + 2*8]" newLine
     tab "MOV RBX, qword [RBX + " major "*8]" newLine
     tab "MOV RBX, qword [RBX + " minor "*8]" newLine
     tab "MOV qword [RBX], RAX" newLine)))

(define cg-set-pvar
  (lambda (var minor)
    (string-append
     tab "MOV qword [rbp + " (+ 4 minor) "*8], RAX" newLine)))

(define cg-set-fvar
  ;;(set! (fvar var) value)
  ;; RAX = [|value|]
  (lambda (var)
    (string-append
     tab "MOV qword [" fvar-label (number->string (f-table-get var)) "], RAX" newLine)))


(define cg-define
  (lambda (var value)
    (let ((address (number->string (f-table-get var f-table))))
    (string-append (code-gen value) newLine
		   tab "MOV qword [" address "], RAX" newLine
		   tab "MOV RAX, " sobVoid newLine))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pre-Text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define param-get-def (string-append
		       newLine
		       ";;; Parameter Getters" newLine
		       newLine
		       "%define param(offset) qword [rbp + offset]" newLine
		       newLine
		       "struc scmframe" newLine
		       ".old_rbp: resq 1" newLine
		       ".ret_addr: resq 1" newLine
		       ".env: resq 1" newLine
		       ".arg_count: resq 1" newLine
		       ".A0: resq 1" newLine
		       ".A1: resq 1" newLine
		       ".A2: resq 1" newLine
		       ".A3: resq 1" newLine
		       ".A4: resq 1" newLine
		       ".A5: resq 1" newLine
		       "endstruc" newLine
		       newLine
		       "%define old_rbp param(scmframe.old_rbp)" newLine
		       "%define ret_addr param(scmframe.ret_addr))" newLine
		       "%define env param(scmframe.env)" newLine
		       "%define arg_count param(scmframe.arg_count))" newLine
		       "%define A0 param(scmframe.A0)" newLine
		       "%define A1 param(scmframe.A1)" newLine
		       "%define A2 param(scmframe.A2)" newLine
		       "%define A3 param(scmframe.A3)" newLine
		       "%define A4 param(scmframe.A4)" newLine
		       "%define A5 param(scmframe.A5)" newLine
		       "%define An(n) qword [rbp + 8*(n+4)]" newLine
		       newLine
		       ))

(define generate-pre-text
  (lambda (ct ft)
    ;;(display (format "Generating Prolog\n"))
    (string-append "%include \"scheme.s\"" newLine
		   ;; param-get-def
		   newLine
		   "section .bss" newLine
		   "global main" newLine
		   newLine
		   "section .data" newLine
		   "start_of_data:" newLine
		   newLine
		   (cg-f-table ft)
		   (cg-c-table ct)
		   "section .text" newLine
		   newLine
		   "main:" newLine)))

(define p-format "%d")

(define print-register
  (let ((l-print "print_register"))
    (string-append "%macro " l-print " 2" newLine
		   tab "MOV rdi, %2" newLine
		   tab "MOVzx rsi, %1" newLine
		   tab "call printf" newLine
		   "%endmacro" newLine
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Post-Text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l-exit "L_exit")

(define post-text
  ;;(begin
    ;;(display (format "Generating Epilogue\n"))
    (string-append l-exit ":" newLine
		   ;;tab "PUSH RAX" newLine
		   ;;tab "call write_sob" newLine
		   tab "ret" newLine));;)
