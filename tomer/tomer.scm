;;;;;;;
;;;;;;;;    change filename to compiler.scm
(load "pc.scm")
(load "qq.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <Whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))

	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))




(define <sexpr-comment>
  (new (*parser (word "#;"))

       (*delayed (lambda ()  <Sexpr>))

       (*caten 2)

       done))

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))

(define <skip>
  (disj <comment>
	<Whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	     (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

(define skipped-char (lambda (ch)
                        (^<skipped*> (char ch))))

(define <Boolean>
    (new (*parser (char #\#))
         (*parser (char #\f))
         (*caten 2)
         (*pack-with
            (lambda (x y) #f))
         (*parser (char #\#))
         (*parser (char #\t))
         (*caten 2)
         (*pack-with
            (lambda (x y) #t))
         (*disj 2)
        done))

(define <CharPrefix>
  (new (*parser (char #\#))
	(*parser (char #\\))
	(*caten 2)
	done))

(define <VisibleSimpleChar>
       (new (*parser <any-char>)
            (*parser (range (integer->char 0) (integer->char 32)))
            *diff
            done))


(define <NamedChar> ;missing page & return
 (new
  (*parser (word-ci "lambda"))   (*pack (lambda (cp)
                     (integer->char 955)))
  (*parser (word-ci "newline"))  (*pack (lambda (cp)
                     (integer->char 10)))
  (*parser (word-ci "nul"))      (*pack (lambda (cp)
                     (integer->char 0)))
  (*parser (word-ci "page"))     (*pack (lambda (cp)
                     (integer->char 12)))
  (*parser (word-ci "return"))   (*pack (lambda (cp)
                     (integer->char 13)))
  (*parser (word-ci "space"))    (*pack (lambda (cp)
                     (integer->char 32)))
  (*parser (word-ci "tab"))      (*pack (lambda (cp)
                     (integer->char 9)))
  (*disj 7)
  done))


  (define <HexDigit>
  (let ((zero (char->integer #\0))
	(lc-a (char->integer #\a))
	(uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
	 (*pack
	  (lambda (ch)
	    (- (char->integer ch) zero)))
	 (*parser (range #\a #\f))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) lc-a))))
	 (*parser (range #\A #\F))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) uc-a))))
	 (*disj 3)
	 done)))

(define <HexChar>

    (new (*parser (range #\0 #\9))
	 (*parser (range #\a #\f))
	 (*parser (range #\A #\F))
	 (*disj 3)
	 done))

(define <HexUnicodeChar>
    (new (*parser (char #\x))
         (*parser <HexDigit>) *plus
	 (*caten 2)
         (*pack-with
            (lambda (x y)
                 (fold-left (lambda (a b)
                        (+ b (* a 16))) 0 y)))
    done))

(define <Char>
    (new (*parser <CharPrefix>)
	 (*parser <HexUnicodeChar>)
         (*parser <VisibleSimpleChar>)
         (*parser <NamedChar>) (*pack (lambda (x) (list->string (list x))))
         (*disj 3)
         (*guard (lambda (n) (or (not (integer? n)) (< n 1114111))))
         (*pack (lambda (n) (if (integer? n)
                                (integer->char n)
                                n)))
         (*caten 2)
         (*pack-with (lambda (x y) y))

         done))

(define <Natural>
    (new (*parser (range #\0 #\9))
	 (*parser (range #\0 #\9)) *star
	 (*caten 2)
	 (*pack-with (lambda (x y) (string->number (list->string  `(,x ,@y)))))
	 done))

(define <Integer>
    (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with (lambda (++ n)  n))
       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with (lambda (-- n) (- n)))
       (*parser <Natural>)
       (*disj 3)
       done))







(define <Fraction>
    (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
	(lambda (x y z)
	  (/ x z)))
       done))

(define <Number>
    (new
	  (*parser <Fraction>)
	  (*parser <Integer>)
	  (*disj 2)
	  done))

(define <StringLiteralChar>
    (new (*parser <any-char>)
	 (*parser (char #\\))
	 (*parser (char #\"))
	 (*disj 2)
	 *diff
	 done))

(define ^<meta-char>
    (lambda (str ch)
        (new (*parser (word str))
             (*pack (lambda(_) ch))
             done)))

(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page))
       (*parser (^<meta-char> "\\\\" #\\))
       (*disj 6)
       done))


(define <StringHexChar>
    (new (*parser (word "\\x"))
         (*parser <HexDigit>)
         *star
         (*parser (char #\;))
         (*caten 3)
         (*pack-with
            (lambda (x y z)
                  (fold-left (lambda (a b)
                        (+ b (* a 16))) 0 y)))
                        (*guard (lambda(n) (< n 1114111)))
                        (*pack (lambda(n) (integer->char n)))
                        done))


(define <StringChar>
    (new
         (*parser <StringHexChar>)
         (*parser <StringMetaChar>)
         (*parser <StringLiteralChar>) ;(*pack (lambda (x) (list->string (list x))))
         (*disj 3)
         done))

(define <String>
    (new (*parser (char #\"))
         (*parser <StringChar>) *star
         (*parser (char #\"))
         (*caten 3)
         (*pack-with (lambda (x y z) (list->string y )))
         done))

(define <SymbolChar>
  (new (*parser (range #\0 #\9))
	 (*parser (range #\a #\z))
	 (*parser (range #\A #\Z))
	 (*pack (lambda (n) (integer->char (+ (char->integer n) 32))))
	 (*parser (char #\!))
	 (*parser (char #\$))
	 (*parser (char #\^))
	 (*parser (char #\*))
	 (*parser (char #\-))
	 (*parser (char #\_))
	 (*parser (char #\=))
	 (*parser (char #\+))
	 (*parser (char #\<))
	 (*parser (char #\>))
	 (*parser (char #\?))
	 (*parser (char #\/))
	 (*disj 15)
	 done))

(define <Symbol>
    (new (*parser <SymbolChar>)
         *plus
         (*pack (lambda (x) (string->symbol (list->string x))))
         done))


(define <InfixPrefixExtensionPrefix>
    (new (*parser (word "##"))
         (*parser (word "#%")) (*disj 2)
	 done))

(define <InfixSymbol>
    (new (*parser <SymbolChar>)
         (*parser (char #\+))
         (*parser (char #\-))
         (*parser (char #\*))
         (*parser (char #\^))
         (*parser (char #\/))
         (*disj 5)
         *diff
         *plus
         (*pack (lambda (x) (string->symbol (list->string x))))
         done))


(define <PowerSymbol>
    (new (*parser (char #\^))
	 (*parser (char #\*))
	 (*parser (char #\*))
	 (*caten 2)
	 (*pack-with (lambda (x y) (list->string (list x y))))
	 (*disj 2)
	done))





(define <ProperList>
    (new (*parser (char #\())
         (*delayed (lambda() <Sexpr>)) *star
         (*parser (char #\)))
        (*caten 3)
        (*pack-with (lambda(x y z) y))
    done))

(define <ImproperList>
    (new (*parser (char #\())
         (*delayed (lambda() <Sexpr>)) *plus
         (*parser (char #\.))
         (*delayed (lambda() <Sexpr>))
         (*parser (char #\)))
         (*caten 5)
         (*pack-with (lambda (op lst dot el cl)  (append lst el)))
         done))

(define <Vector>
    (new (*parser (char #\#))
         (*parser (char #\())
         (*delayed (lambda() <Sexpr>)) *star
         (*parser (char #\)))
         (*caten 4)
         (*pack-with (lambda(x y z w) (list->vector z)))
         done))

(define <Quoted>
    (new (*parser  (char #\'))
         (*delayed (lambda() <Sexpr>))
         (*caten 2)
         (*pack-with (lambda (x y) `',y))
        done))

(define <QuasiQuoted>
    (new (*parser  (char #\`))
         (*delayed (lambda() <Sexpr>))
         (*caten 2)
         (*pack-with (lambda (x y) (list 'quasiquote y)))
         done))

(define <Unquoted>
    (new (*parser (char #\,))
         (*delayed (lambda() <Sexpr>))
         (*caten 2)
         (*pack-with (lambda(x y) (list 'unquote y)))
         done))

(define <UnquoteAndSpliced>
    (^<skipped*> (new (*parser (word ",@"))
         (*delayed (lambda() <Sexpr>))
         (*caten 2)
         (*pack-with (lambda(x y) (list 'unquote-splicing y)))
         done)))







(define <InfixPow>    (^<skipped*> (new
         (*delayed (lambda () <InfixAtom>))
         (*parser (^<skipped*> <PowerSymbol>))
         (*delayed (lambda () <InfixAtom>))
         (*caten 2)
         (*pack-with (lambda(x y) y))
          *star
         (*caten 2)
         (*pack-with (lambda(a lst) (fold-right (lambda (num next) `(expt ,num  ,next)) (car (reverse (cons a lst)))  (reverse(cdr (reverse (cons a lst)))))))
         done)))

;; (define <InfixDiv> (^<skipped*>
;;     (new (*delayed (lambda () <InfixDiv>))
;;
;;          (*delayed (lambda () <InfixDiv>))
;;          (*caten 2)
;;          (*pack-with (lambda(x y) (cons x y)))
;;           *star
;;          (*caten 2)
;;          (*pack-with (lambda(a lst) (fold-left (lambda (num next) `(/ ,num ,(cdr next))) a lst)))
;;          done)))

(define <DivMul> (^<skipped*>
    (new (*delayed (lambda () <InfixPow>))
         (*parser (skipped-char #\*))
         (*parser (skipped-char #\/))
         (*disj 2)
         (*delayed (lambda () <InfixPow>))
         (*caten 2)
         (*pack-with (lambda(x y) (cons x y)))
          *star
         (*caten 2)
         (*pack-with (lambda(a lst) (fold-left (lambda (num next) `(,(string->symbol (string (car next))) ,num ,(cdr next))) a lst)))
         done)))

;(define <DivMul> (^<skipped*> (new (*parser <InfixMul>) (*parser <InfixDiv>) (*disj 2) done)))

(define <InfixSub> (^<skipped*>
    (new (*delayed (lambda () <DivMul>))
         (*parser (skipped-char #\-))
         (*delayed (lambda () <DivMul>))
         (*caten 2)
         (*pack-with (lambda(fun num) (cons fun num)))
          *star
         (*caten 2)
         (*pack-with (lambda(a lst) (fold-left (lambda (num next) `(- ,num ,(cdr next))) a lst)))
         done)))

(define <AddSub> (^<skipped*>
    (new (*delayed (lambda () <DivMul>))
         (*parser (skipped-char #\+))
         (*parser (skipped-char #\-))
         (*disj 2)
         (*delayed (lambda () <DivMul>))
         (*caten 2)
         (*pack-with (lambda(fun num) (cons fun num)))
          *star
         (*caten 2)
         (*pack-with (lambda(a lst)  (fold-left (lambda (num next) `(,(string->symbol (string (car next))) ,num ,(cdr next))) a lst)))
         done)))

;(define <AddSub> (^<skipped*> (new (*parser <InfixAdd>) (*parser <InfixSub>) (*disj 2) done)))

(define <ClassicExpression> (^<skipped*> (new (*parser <AddSub>) done)))

;; (define <InfixArgList> (new (*parser <ClassicExpression>)
;;                     (*parser (char #\,))
;;                     (*parser <ClassicExpression>)
;;                      (*caten 2) (*pack (lambda (x) (cadr x))) *star
;;                      (*parser <epsilon>)
;;                      (*disj 2)
;;                      (*caten 2)
;;                      (*pack-with (lambda (x y) `(,x ,@y)))
;;                      done))


(define <InfixArgList>
   (^<skipped*> (new (*parser <ClassicExpression>)
         (*parser (skipped-char #\,))
         (*parser <ClassicExpression>)
         (*caten 2)
         (*pack-with (lambda(x y) y))
         *star
         (*caten 2)
         (*pack-with (lambda(x y) `(,x ,@y)))
         done)))


(define <InfixSexprEscape>  (^<skipped*> (new (*parser <InfixPrefixExtensionPrefix> )
    (*parser (*delayed (lambda () <Sexpr>)))
    (*caten 2)
    (*pack-with (lambda (x y) y))
    done)))

(define <terminalo>
    (^<skipped*> (new (*parser <Number>)
         (*parser <InfixSymbol>)
         (*parser <InfixSexprEscape>)
         (*disj 3)
         done)))



(define <InfixFuncall>
    (^<skipped*> (new (*delayed (lambda() <InfixParen>))
         (*parser (skipped-char #\())
         (*delayed (lambda() <InfixArgList>))
         (*parser <epsilon>)
         (*disj 2)
         (*parser (skipped-char #\)))
         (*caten 3)
         (*pack-with (lambda (x y z) y))
         *star
         (*caten 2)
         (*pack-with (lambda (a lst) (fold-left (lambda (exp next) `(,exp ,@next)) a lst)))
         (*delayed (lambda() <InfixParen>))
         (*disj 2)
         done)))




(define <InfixParen>
     (^<skipped*> (new (*parser (char #\-))
         (*parser <Number>)
         (*caten 2)
         (*pack-with (lambda (a b) (- b)))
         (*parser (char #\-))
         (*parser <DivMul>)
         (*caten 2)
         (*pack-with (lambda (a b) `(- ,b)))
         (*parser (skipped-char #\())
         (*delayed (lambda() <AddSub>))
         (*parser (skipped-char #\)))
         (*caten 3)
         (*pack-with (lambda (x y z) y))
         (*delayed (lambda() <terminalo>))
         (*disj 4)
         done)))


(define <InfixAtom>
        (^<skipped*> (new (*delayed (lambda() <InfixFuncall>))
         (*parser (skipped-char #\[))
         (*delayed (lambda() <AddSub>))
         (*parser (skipped-char #\]))
         (*caten 3)
         (*pack-with (lambda(x y z) y))
         *star
         (*caten 2)
         (*pack-with (lambda(x y) (fold-left (lambda (exp nxt) `(vector-ref ,exp ,nxt)) x y)))
         done)))


(define <InfixExtension>
  (^<skipped*>
    (new (*parser <InfixPrefixExtensionPrefix>)
         (*parser  <AddSub>)
         (*caten 2)
         (*pack-with (lambda(x y) y))
         done)))

(define <Sexpr>
   (^<skipped*>
     (new
          (*parser <Boolean>)
          (*parser <Number>)
          (*parser <Symbol>)
          (*parser (range #\0 #\9))
          *diff
          *not-followed-by
          (*parser <Char>)
          (*parser <String>)
          (*parser <Symbol>)
          (*parser <ProperList>)
          (*parser <ImproperList>)
          (*parser <Vector>)
          (*parser <Quoted>)
          (*parser <QuasiQuoted>)
          (*parser <Unquoted>)
          (*parser <UnquoteAndSpliced>)
          (*parser <InfixExtension>)
          (*disj 13)
     done)))

     (define <sexpr> <Sexpr>)

;;;;;;;HW2
(define params-getter (lambda (lmd)
    (cond ((equal? 'lambda-simple (car lmd)) (cadr lmd))
	  ((equal? 'lambda-var (car lmd)) (list (cadr lmd)))
	  ((equal? 'lambda-opt (car lmd))  (append (cadr lmd) (list (caddr lmd))))
	  (else '()))))


(define body-getter (lambda (lmd)
    (cond ((or (equal? 'lambda-simple (car lmd)) (equal? 'lambda-var (car lmd))) (caddr lmd))
	  (else (cadddr lmd)))))

(define make-begin
      (lambda (lst)
	    (cond ((= 0 (length lst)) `(const ,@(list (void))))
		  ((= 1 (length lst)) (car lst))
		  (else `(begin ,@lst)))))

(define not-contains-doubles?
        (letrec ((not-car-in-cdr? (lambda (a subl)
                        (cond ((null? subl) #t)
                              ((equal? a (car subl)) #f)
                              (else (not-car-in-cdr? a (cdr subl)))))))
        (lambda (input-list)
            (cond ((null? input-list) #t)
                ((not-car-in-cdr? (car input-list) (cdr input-list)) (not-contains-doubles? (cdr input-list)))
                (else #f)))))

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define const_?
    (lambda (c)
        (or (null? c) (vector? c) (boolean? c) (char? c) (number? c) (string? c) (quote? c))))

(define parse-quote
    (lambda (q)
        `(const ,(cadr q))))

(define parse-const
    (lambda (c)
        (if (quote? c) (parse-quote c)
            `(const ,c))))

(define not-reserved?
    (letrec ((not-in-list? (lambda (x lst)
                (cond ((null? lst) #t)
                      ((equal? (car lst) x) #f)
                      (else (not-in-list? x (cdr lst)))))))
    (lambda (x)
        (not-in-list? x *reserved-words*))))

(define var?
    (lambda (v)
        (and (symbol? v) (not-reserved? v))))


(define parse-var
    (lambda (v)
        `(var ,v)))

(define if3?
    (lambda (sxpr)
        (and (list? sxpr) (or (= 4 (length sxpr)) (= 3 (length sxpr)) ) (equal? 'if (car sxpr)) )))


(define parse-if3
     (lambda (sxpr)
        (if (= 4 (length sxpr))
            `(if3 ,(morgan (cadr sxpr)) ,(morgan (caddr sxpr)) ,(morgan (cadddr sxpr)))
            `(if3 ,(morgan (cadr sxpr)) ,(morgan (caddr sxpr)) ,`(const ,@(list (void)))))))

(define and?
    (lambda (sxpr)
                (and (list? sxpr) (< 0 (length sxpr)) (equal? 'and (car sxpr)))))

(define parse-and
    (letrec ((rex (lambda (lst)
            (if (= 1 (length lst)) (morgan (car lst))
                `(if3 ,(morgan (car lst)) ,(rex (cdr lst)) ,(morgan #f))))))
    (lambda (sxpr)
        (if (= 1 (length sxpr)) (morgan #t)
            (rex (cdr sxpr))))))



(define applic?
    (lambda (sxpr)
        (and (list? sxpr) (< 0 (length sxpr)) (not-reserved? (car sxpr)))) )

(define parse-applic
    (lambda (sxpr)
        `(applic ,(morgan (car sxpr)) ,(map morgan (cdr sxpr)))))


(define or?
    (lambda (sxpr)
        (and (list? sxpr) (< 0 (length sxpr)) (equal? 'or (car sxpr)))))

(define parse-or
    (lambda (sxpr)
        (cond ((= 1 (length sxpr)) (morgan #f))
              ((= 2 (length sxpr)) (morgan (cadr sxpr)))
             (else `(or ,(map morgan (cdr sxpr)))))))

(define lambda-simple?
    (letrec ((proper-list-of-vars? (lambda (lst)
                (cond ((null? lst) #t)
                      ((var? (car lst)) (proper-list-of-vars? (cdr lst)))
                      (else #f)))))
    (lambda (sxpr)
        (and (list? sxpr) (< 2 (length sxpr)) (equal? 'lambda (car sxpr)) (list? (cadr sxpr)) (not-contains-doubles? (cadr sxpr)) (proper-list-of-vars? (cadr sxpr))))))

(define parse-lambda-simple
    (lambda (sxpr)
        `(lambda-simple ,(cadr sxpr) ,(morgan (make-begin (cddr sxpr))))))

(define lambda-opt?
    (letrec ((improper-list-of-vars? (lambda (lst)
                (cond ((null? (cdr lst)) #f)
                      ((and (var? (car lst)) (var? (cdr lst))) #t)
                      ((and (var? (car lst)) (var? (cadr lst))) (improper-list-of-vars? (cdr lst)))
                      (else #f)))))
    (lambda (sxpr)
        (and (list? sxpr) (< 2 (length sxpr)) (equal? 'lambda (car sxpr)) (pair? (cadr sxpr)) (improper-list-of-vars? (cadr sxpr))))))

(define parse-lambda-opt
    (letrec ((pre-dot (lambda (lst)
                (if (var? (cdr lst)) (list (car lst))
                    (cons (car lst) (pre-dot (cdr lst))))))
             (post-dot (lambda (lst)
                (if (var? (cdr lst)) (cdr lst)
                    (post-dot (cdr lst))))))
    (lambda (sxpr)
        `(lambda-opt ,(pre-dot (cadr sxpr)) ,(post-dot (cadr sxpr)) ,(morgan (make-begin (cddr sxpr)))))))

(define lambda-var?
    (lambda (sxpr)
        (and (list? sxpr) (< 2 (length sxpr)) (equal? 'lambda (car sxpr)) (var? (cadr sxpr)))))

(define parse-lambda-var
    (lambda (sxpr)
        `(lambda-var ,(cadr sxpr) ,(morgan (make-begin (cddr sxpr))))))

(define def-reg?
    (lambda (sxpr)
        (and (list? sxpr) (= 3 (length sxpr)) (equal? 'define (car sxpr)) (var? (cadr sxpr)))))

(define parse-def-ref
    (lambda (sxpr)
        `(def ,(morgan (cadr sxpr)) ,(morgan (caddr sxpr)))))

(define def-mit?
    (letrec ((proper-list-of-vars? (lambda (lst)
                (cond ((null? lst) #t)
                      ((var? (car lst)) (proper-list-of-vars? (cdr lst)))
                      (else #f))))
            (improper-list-of-vars? (lambda (lst)
                (cond ((null? (cdr lst)) #f)
                      ((and (var? (car lst)) (var? (cdr lst))) #t)
                      ((and (var? (car lst)) (var? (cadr lst))) (improper-list-of-vars? (cdr lst)))
                      (else #f)))))
    (lambda (sxpr)
        (and (list? sxpr) (< 2 (length sxpr)) (equal? 'define (car sxpr)) (or (and (list? (cadr sxpr)) (< 0 (length (cadr sxpr))) (proper-list-of-vars? (cadr sxpr))) (and (pair? (cadr sxpr)) (improper-list-of-vars? (cadr sxpr))))))))



(define parse-def-mit
    (let ((var-lst (lambda (lst)
                (cadr lst)))
          (proc-name (lambda (lst)
                (morgan (car lst))))
          (proc-vars (lambda (lst)
                (cdr lst)))
          (proc-body (lambda (lst)
                (cddr lst))))
    (lambda (sxpr)
        `(def ,(proc-name (var-lst sxpr))  ,(morgan `(lambda ,(proc-vars (var-lst sxpr)) ,@(proc-body sxpr)))))))

 (define let?
    (letrec ((lop? (lambda (lst)
	      (cond ((null? lst) #t)
		    ((and (pair? (car lst)) (var? (caar lst))) (lop? (cdr lst)))
		    (else #f))))
            (proc-vars (lambda (lst)
		    (if (null? lst) '()
			 (cons (caar lst) (proc-vars (cdr lst)))))))
    (lambda (sxpr)
	  (and (list? sxpr) (< 2 (length sxpr)) (equal? 'let (car sxpr)) (list? (cadr sxpr)) (lop? (cadr sxpr)) (not-contains-doubles? (proc-vars (cadr sxpr))) ))))

 (define parse-let
      (letrec ((proc-vars (lambda (lst)
		    (if (null? lst) '()
			 (cons (caar lst) (proc-vars (cdr lst))))))
		(proc-exps (lambda (lst)
		    (if (null? lst) '()
			  (cons (cadar lst) (proc-exps (cdr lst))))))
		(lambda-expr (lambda (sxpr) `(lambda ,(proc-vars (cadr sxpr)) ,@(cddr sxpr)))))
	(lambda (sxpr)
	    (morgan `( ,(lambda-expr sxpr) ,@(proc-exps (cadr sxpr)))))))

 (define let-star?
    (letrec ((lop? (lambda (lst)
	      (cond ((null? lst) #t)
		    ((and (pair? (car lst)) (var? (caar lst))) (lop? (cdr lst)))
		    (else #f)))))
    (lambda (sxpr)
	  (and (list? sxpr) (< 2 (length sxpr)) (equal? 'let* (car sxpr)) (list? (cadr sxpr)) (lop? (cadr sxpr))))))

(define parse-let-star
    (letrec ;((args (lambda (sxpr) (cadr sxpr)))
	    ;(body (lambda (sxpr) (cddr sxpr)))
	    ((nested-let (lambda (args body)
		    (if (= 1 (length args))
			  `(let ,args ,@body)
			  `(let ,(list (car args)) ,(nested-let (cdr args) body))))))
    (lambda (sxpr)
	  (if (= 0 (length (cadr sxpr))) (morgan `(let ,(list) ,@(cddr sxpr)))
	  (morgan (nested-let (cadr sxpr) (cddr sxpr)))))))

(define set?
    (lambda (sxpr)
	(and (list? sxpr) (= 3 (length sxpr)) (equal? 'set! (car sxpr)) (var? (cadr sxpr)))))

(define parse-set
    (lambda (sxpr)
	`(set ,(morgan (cadr sxpr)) ,(morgan (caddr sxpr)))))

(define letrec?
	(letrec ((lop? (lambda (lst)
	      (cond ((null? lst) #t)
		    ((and (pair? (car lst)) (var? (caar lst))) (lop? (cdr lst)))
		    (else #f)))))
    (lambda (sxpr)
	  (and (list? sxpr) (< 2 (length sxpr)) (equal? 'letrec (car sxpr)) (list? (cadr sxpr)) (lop? (cadr sxpr))))))

(define parse-letrec
	(letrec ((proc-vars (lambda (lst)
		    (if (null? lst) '()
			 (cons (caar lst) (proc-vars (cdr lst))))))
		(proc-exps (lambda (lst)
		    (if (null? lst) '()
			  (cons (cadar lst) (proc-exps (cdr lst))))))
		(proc-body (lambda (sxpr)
		      `(lambda ,(list) ,@(cddr sxpr))))
		(proc-whatever (lambda (sxpr)
		      `(,(proc-body sxpr) ))))
	(lambda (sxpr)
	      (morgan `(let ,(map (lambda (v) (list v #f)) (proc-vars (cadr sxpr)))
			    ,@(map (lambda (var val) `(set! ,var ,val)) (proc-vars (cadr sxpr)) (proc-exps (cadr sxpr)))
			     ,(proc-whatever sxpr))))))

(define cond?
      (letrec ((rex (lambda (lst)
		  (cond ((and (= 1 (length lst)) (pair? (car lst))) #t)
			((and (pair? (car lst)) (not (equal? 'else (caar lst))) (rex (cdr lst))))
			(else #f)))))
     (lambda (sxpr)
	  (and (list? sxpr) (< 1 (length sxpr)) (equal? 'cond (car sxpr)) (< 0 (length (cdr sxpr))) (rex (cdr sxpr))))))

(define parse-cond
	(letrec ((rex (lambda (lst)
		  (cond ((and (= 1 (length lst)) (equal? 'else (caar lst) ))  (make-begin (cdar lst)))
			(( = 1 (length lst)) `(if ,(caar lst) ,(make-begin (cdar lst) )))
			(else `(if ,(caar lst) ,(make-begin (cdar lst)) ,(rex (cdr lst))))))))
	(lambda (sxpr)
	    (morgan (rex (cdr sxpr))))))

(define begin?
    (lambda (sxpr)
	(and (list? sxpr) (< 0 (length sxpr)) (equal? 'begin (car sxpr)))))

(define parse-begin
      (letrec ((rex (lambda (lst)
		  (cond ((null? lst) (list))
			((begin? (car lst)) (append (rex (cdr (car lst))) (rex (cdr lst))))
			(else (cons (morgan (car lst) ) (rex (cdr lst))))))))
    (lambda (sxpr)
	(cond ((= 1 (length sxpr)) `(const ,@(list (void))))
	      ((= 2 (length sxpr)) (morgan (cadr sxpr)))
	      (else `(seq ,(rex (cdr sxpr))))))))

(define quasiquote?
    (lambda (sxpr)
	  (and (list? sxpr) (= 2 (length sxpr)) (equal? 'quasiquote (car sxpr)))))

(define parse-quasiquote
    (lambda (sxpr)
	(morgan (expand-qq (cadr sxpr)))))


(define def-to-letrec?
      (letrec ((define-in-body (lambda (lst)
			(cond ((null? lst) '())
                              ((equal? 'seq (car lst)) (define-in-body (cadr lst)))
                              ((and (list? (car lst)) (not (null? (car lst))) (equal? 'def (caar lst))) (cons (cdr (car lst)) (define-in-body (cdr lst))))
                              (else (define-in-body (cdr lst))))))
		)
      (lambda (sxpr)
	  (if (and (list? sxpr) (< 2 (length sxpr)) (or (equal? 'lambda-simple (car sxpr))
		  (equal? 'lambda-opt (car sxpr))
		  (equal? 'lambda-var (car sxpr))))
	      (if (null? (define-in-body (body-getter sxpr))) #f
			#t) ;`(,(car sxpr) ,(cadr sxpr) (letrec ,(define-in-body (body sxpr)) ,(eliminated-body  (body sxpr)))))
	      #f))))

(define parse-def-to-letrec
      (letrec ((define-in-body (lambda (lst)
			(cond ((null? lst) '())
                              ((equal? 'seq (car lst)) (define-in-body (cadr lst)))
                              ((and (list? (car lst)) (not (null? (car lst))) (equal? 'def (caar lst))) (cons (cons 'set (cdr (car lst))) (define-in-body (cdr lst))))
                              (else (define-in-body (cdr lst))))))
		(eliminated-body (lambda (lst)
				(cond ((null? lst) '())
                                      ((equal? 'seq (car lst)) (cons 'seq (list (eliminated-body (cadr lst)))))
                                      ((and (list? (car lst)) (not (null? (car lst))) (equal? 'def (caar lst))) (cons `(set ,@(cdr (car lst))) (eliminated-body (cdr lst))))
				      (else (cons (car lst) (eliminated-body (cdr lst)))))))
                (listvars (lambda (lst)
                            (cond ((null? lst) '())
                                  (else (cons (cadr (cadr (car lst))) (listvars (cdr lst)))))))
                 (header (lambda (sxpr) (if (equal? 'lambda-opt (car sxpr)) (list (car sxpr) (cadr sxpr) (caddr sxpr)) (list (car sxpr) (cadr sxpr)))))
                )
      (lambda (sxpr)
	  (eliminate-nested-defines   `(,@(header sxpr) (applic (lambda-simple ,(listvars (define-in-body (body-getter sxpr)))   ,(eliminated-body  (body-getter sxpr)))
                                                        ,(map (lambda (x) `(const #f))  (listvars (define-in-body (body-getter sxpr))))))))))

;; (define el-define-switch #f)
;; (define el-res "error1: expression not parsed") ;;issue to solve - if eliminate activated after regualr parse the result will be incorrect
;; (define el-res-set (lambda (sxpr)
;; 	     (set! el-res sxpr)))
;;
;; DO NOT DELETE
(define morgan
    (lambda (sxpr)
	;(begin (el-res-set sxpr)
	  (cond
		((const_? sxpr) (parse-const sxpr))
		((var? sxpr) (parse-var sxpr))
		((if3? sxpr) (parse-if3 sxpr))
		((and? sxpr) (parse-and sxpr))
		((or? sxpr) (parse-or sxpr))
		((lambda-simple? sxpr) (parse-lambda-simple sxpr))
		((lambda-opt? sxpr) (parse-lambda-opt sxpr))
		((lambda-var? sxpr) (parse-lambda-var sxpr))
		((def-reg? sxpr) (parse-def-ref sxpr))
		((def-mit? sxpr) (parse-def-mit sxpr))
		((set? sxpr) (parse-set sxpr))
		((let? sxpr) (parse-let sxpr))
		((let-star? sxpr) (parse-let-star sxpr))
		((letrec? sxpr) (parse-letrec sxpr))
		((cond? sxpr) (parse-cond sxpr))
		((begin? sxpr) (parse-begin sxpr))
		((applic? sxpr) (parse-applic sxpr))
		((quasiquote? sxpr) (parse-quasiquote sxpr))
		(else "ERROR"))))

(define parse (lambda (sxpr)
	 (morgan sxpr)))


;HW3

;; (define *test-expr*
;;     '(define my-even?
;; 	(lambda (e)
;; 	  (define even? (lambda (n) (or (zero? n) (odd? (- n 1)))))
;; 	  (define odd?
;; 	      (lambda (n) (and (positive? n) (even? (- n 1)))))
;; 	  (even? e))))

;;;;;;;;;;;;;;;;;;;;;;;;;DRAFT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define def-to-letrec
;;       (letrec ((rex (lambda (lst)
;; 		  (cond ((null? lst) (list))
;; 			((begin? (car lst)) (append (rex (cdr (car lst))) (rex (cdr lst))))
;; 			(else (cons (car lst)  (rex (cdr lst)))))))
;; 	       (body (lambda (sxpr) (rex (make-begin (cddr sxpr)))))
;; 	       (define-in-body (lambda (lst)
;; 			(cond ((null? lst) '())
;; 			      ((def-reg? (car lst)) (cons (cdr (car lst)) (define-in-body (cdr lst))))
;; 			      ((def-mit? (car lst)) (cons
;; 				      `( ,(car (cadr (car lst))) (lambda ,(cdr (cadr (car lst)))
;; 					  (cddr lst))) (define-in-body (cdr lst))))
;; 			      (else (define-in-body (cdr lst))))))
;; 		(eliminated-body (lambda (lst)
;; 				(cond ((null? lst) '())
;; 				      ((or (def-reg? (car lst)) (def-mit? (car lst))) (eliminated-body (cdr lst)))
;; 				      (else (cons (car lst) (eliminated-body (cdr lst))))))))
;;       (lambda (sxpr)
;; 	  (if (or (lambda-simple? sxpr)
;; 		  (lambda-opt? sxpr)
;; 		  (lambda-var? sxpr)
;; 		  (def-mit? sxpr)
;; 		  (let? sxpr)
;; 		  (let-star? sxpr)
;; 		  (letrec? sxpr))
;; 	      (if (null? (define-in-body (body sxpr))) sxpr
;; 			`(,(car sxpr) ,(cadr sxpr) (letrec ,(define-in-body (body sxpr)) ,(eliminated-body  (body sxpr)))))
;; 	      sxpr))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;; (define def-el-no-idea 'lala)
;; (define pullswitch-el-define
;; 	(lambda ()
;; 	      (begin (set! el-define-switch #t) (set! def-el-no-idea (morgan el-res)) (set! el-res "error2: expression not parsed") (set! el-define-switch #f))))
;;
;;
;; (define eliminate-nested-defines
;; 	(lambda (obj)
;; 	      (begin (pullswitch-el-define) def-el-no-idea)))

(define eliminate-nested-defines (lambda (sxpr)
            (cond ((null? sxpr) '())
                  ((def-to-letrec? sxpr) (parse-def-to-letrec sxpr))
                  ((list? sxpr) (cons (eliminate-nested-defines (car sxpr)) (eliminate-nested-defines (cdr sxpr))))
                  (else sxpr))))

;; HW3- part 4


(define reduntunt-lambda-applic?
      (lambda (sxpr)
	    (and (list? sxpr) (= 3 (length sxpr)) (equal? 'applic (car sxpr)) ;applic content
	    (list? (cadr sxpr)) (= 3 (length (cadr sxpr))) (equal? 'lambda-simple (car (cadr sxpr))) ;lambda-simple content
	    (list? (cadr (cadr sxpr))) (= 0 (length (cadr (cadr sxpr)))) ;lambda-simple-params
	    (list? (caddr sxpr)) (= 0 (length (caddr sxpr))))))

(define parse-reduntunt-lambda-applic
      (lambda (sxpr)
	  (remove-applic-lambda-nil (caddr (cadr sxpr)))))


(define remove-applic-lambda-nil
      (lambda (sxpr)
	  (cond ((null? sxpr) '())
		((reduntunt-lambda-applic? sxpr) (parse-reduntunt-lambda-applic sxpr))
		((list? sxpr) (cons (remove-applic-lambda-nil (car sxpr)) (remove-applic-lambda-nil (cdr sxpr))))
		(else sxpr))))


;;HW3- part 5

(define bound-var?
    (letrec ((v-in-sxpr (lambda (v sxpr)
		  (cond ((null? sxpr) #f)
			((list? sxpr) (or (v-in-sxpr v (car sxpr)) (v-in-sxpr v (cdr sxpr))))
			((equal? v sxpr) #t)
			(else #f))))
	    (v-not-param (lambda (v sxpr) (not (member v (params-getter sxpr)))))
	    (lambda? (lambda (v sxpr) (and (list? sxpr) (< 2 (length sxpr)) (or (and (v-not-param v sxpr) (equal? 'lambda-simple (car sxpr)))
					  (and (v-not-param v sxpr) (equal? 'lambda-var (car sxpr))) (and (v-not-param v sxpr) (equal? 'lambda-opt (car sxpr)))))))
	    (rex (lambda (v sxpr)
		  (cond ((null? sxpr) #f)
			((lambda? v sxpr) (v-in-sxpr v sxpr))
			((list? sxpr) (or (rex v (car sxpr)) (rex v (cdr sxpr))))
			(else #f)))))
    (lambda (v scope)
	(rex v scope))))


(define set-equal? (lambda (v sxpr) (and (list? sxpr) (= 3 (length sxpr)) (equal? 'set (car sxpr)) (equal? v (cadr (cadr sxpr))))))

(define has-set?
(letrec ((v-param (lambda (v sxpr)   (member v (params-getter sxpr))))
	    (lambda? (lambda (v sxpr) (and (list? sxpr) (< 2 (length sxpr)) (or (and  (equal? 'lambda-simple (car sxpr)) (v-param v sxpr))
					  (and (equal? 'lambda-var (car sxpr)) (v-param v sxpr)) (and  (equal? 'lambda-opt (car sxpr)) (v-param v sxpr)))))))
      (lambda (v sxpr)
	    (cond ((null? sxpr) #f)
		  ((lambda? v sxpr) #f)
		  ((set-equal? v sxpr) #t)
		  ((list? sxpr) (or (has-set? v (car sxpr)) (has-set? v (cdr sxpr))))
		  (else #f)))))

(define has-get?
(letrec ((v-param (lambda (v sxpr) (member v (params-getter sxpr))))
	    (lambda? (lambda (v sxpr) (and (list? sxpr) (< 2 (length sxpr)) (or (and (v-param v sxpr) (equal? 'lambda-simple (car sxpr)))
		(and (v-param v sxpr) (equal? 'lambda-var (car sxpr))) (and (v-param v sxpr) (equal? 'lambda-opt (car sxpr))))))))
    (lambda (v sxpr)
	   (cond ((null? sxpr) #f)
		((lambda? v sxpr)  #f)
		 ((set-equal? v sxpr) (has-get? v (caddr sxpr)))
		 ((and (list? sxpr) (= 2 (length sxpr)) (equal? 'var (car sxpr)) (equal? v (cadr sxpr))) #t)
		 ((list? sxpr) (or (has-get? v (car sxpr)) (has-get? v (cdr sxpr))))
		 (else #f)))))

(define box-param?
     (lambda (v scope)
	  (and (bound-var? v scope) (has-set? v scope) (has-get? v scope))))

(define list-of-box-params
    (letrec ((rex (lambda (sxpr lst)
		(cond ((null? lst) '())
		      ((box-param? (car lst) sxpr) (cons (car lst) (rex sxpr (cdr lst))))
		      (else (rex sxpr (cdr lst)))))))
    (lambda (scope list-of-vars)
	  (reverse (rex scope list-of-vars)))))

(define readd-box-param
    (letrec ((v-param (lambda (v sxpr)  (member v (params-getter sxpr))))
	    (lambda-with-param? (lambda (v sxpr) (and (list? sxpr) (< 2 (length sxpr)) (or (and (v-param v sxpr) (equal? 'lambda-simple (car sxpr)))
					 (and (v-param v sxpr) (equal? 'lambda-var (car sxpr))) (and (v-param v sxpr) (equal? 'lambda-opt (car sxpr)))))))
	    (rex (lambda (v sxpr)
		(cond ((null? sxpr) '())
		      ((lambda-with-param? v sxpr) sxpr)
		      ((set-equal? v sxpr) `(box-set ,(cadr sxpr) ,(rex v (caddr sxpr))))
		      ((and (list? sxpr) (= 2 (length sxpr)) (equal? 'var (car sxpr)) (equal? v (cadr sxpr)))
					    `(box-get ,sxpr))
		      ((list? sxpr) (cons (rex v (car sxpr)) (rex v (cdr sxpr))))
		      (else sxpr)))))
    (lambda (v scope)
	(if (equal? 'seq (car scope))
		 `(seq ((set (var ,v) (box (var ,v))) ,@(rex v (cadr scope))))
		 `(seq ((set (var ,v) (box (var ,v))) ,(rex v  scope)))))))

(define box-lambda-expr
      (letrec ((header (lambda (sxpr) (if (equal? 'lambda-opt (car sxpr)) (list (car sxpr) (cadr sxpr) (caddr sxpr)) (list (car sxpr) (cadr sxpr)))))
	      (scope (lambda (sxpr) (if (equal? 'lambda-opt (car sxpr)) (cadddr sxpr) (caddr sxpr))))
	      (params (lambda (sxpr) (params-getter sxpr)))
	      (rex (lambda (scope box-params)
		    (cond ((null? box-params) scope)
			  (else (readd-box-param (car box-params) (rex scope (cdr box-params))))))))
	(lambda (sxpr)

		`( ,@(header sxpr) ,@(list (box-set (rex (scope sxpr) (list-of-box-params (scope sxpr) (reverse (params sxpr))))))))))

(define box-set (lambda (sxpr)
	  (cond  ((null? sxpr) '())
		 ((and (list? sxpr) (< 2 (length sxpr)) (or (equal? 'lambda-opt (car sxpr)) (equal? 'lambda-simple (car sxpr)) (equal? 'lambda-var (car sxpr))))
				(box-lambda-expr sxpr))
		 ((list? sxpr) (cons (box-set (car sxpr)) (box-set (cdr sxpr))))
		 (else sxpr))))


;;HW3- part 6

;(lambda (a b c) (lambda (d e f) (..)))


(define index-slave
      (letrec ((lambda? (lambda (sxpr) (and (list? sxpr) (< 2 (length sxpr)) (or (equal? 'lambda-simple (car sxpr))
					  (equal? 'lambda-var (car sxpr)) (equal? 'lambda-opt (car sxpr))))))
	      (header (lambda (sxpr) (if (equal? 'lambda-opt (car sxpr)) (list (car sxpr) (cadr sxpr) (caddr sxpr)) (list (car sxpr) (cadr sxpr))))))
      (lambda (v adress sxpr lst i)
	 (cond  ((null? sxpr) '())
		((lambda? sxpr) (append (header (enslave-sxpr sxpr)) (list (annotate-scope (body-getter (enslave-sxpr sxpr)) lst (+ i 1)))))

		((and (list? sxpr) (equal? 'var (car sxpr)) (equal? v (cadr sxpr))) adress)
		((list? sxpr) (cons (index-slave v adress (car sxpr) lst i) (index-slave v adress (cdr sxpr) lst i)))
		(else sxpr)))))


(define rename-vars
      (let ((buzz (lambda (qq) (cons (car qq) (cons (car (reverse (cdr qq))) (reverse (cdr (reverse (cdr qq))) ))))))
      (lambda (scope reversed-lst prefix lst i)
	    (cond ((null? reversed-lst) scope)
		  (else (index-slave (car reversed-lst) (buzz (append prefix (list (- (length reversed-lst) 1) (car reversed-lst))) )
			      (rename-vars scope (cdr reversed-lst) prefix lst i) lst i))))))


(define annotate-scope
      (lambda (scope reversed-lst i)
	  (rename-vars scope reversed-lst  `(bvar ,i) reversed-lst i)))

(define enslave-sxpr
	  (let ((lambda? (lambda (sxpr) (and (list? sxpr) (< 2 (length sxpr)) (or (equal? 'lambda-simple (car sxpr)) (equal? 'lambda-var (car sxpr))
					   (equal? 'lambda-opt (car sxpr))))))
		(params (lambda (sxpr) (params-getter sxpr)))
		(header (lambda (sxpr) (if (equal? 'lambda-opt (car sxpr)) (list (car sxpr) (cadr sxpr) (caddr sxpr)) (list (car sxpr) (cadr sxpr))))))
	(lambda (sxpr)
	     (append (header sxpr) (list (rename-vars (body-getter sxpr) (reverse (params sxpr)) `(pvar) (reverse (params sxpr)) -1))) )))

(define annotate-expression (lambda (sxpr)
	  (let ((lambda? (lambda (sxpr) (and (list? sxpr) (< 2 (length sxpr)) (or (equal? 'lambda-simple (car sxpr)) (equal? 'lambda-var (car sxpr))
					   (equal? 'lambda-opt (car sxpr)))))))
	  (cond ((null? sxpr) '())
		((and (lambda? sxpr) (< 0 (length (params-getter sxpr)))) (enslave-sxpr sxpr))
		((list? sxpr) (cons (annotate-expression (car sxpr)) (annotate-expression (cdr sxpr))))
		(else sxpr)))))

(define pe->lex-pe (lambda (sxpr)
      (letrec ((anot (annotate-expression sxpr))
	      (rex (lambda (sxpr)
		      (cond ((null? sxpr) '())
			    ((and (list? sxpr) (= 2 (length sxpr)) (equal? 'var (car sxpr))) (cons 'fvar (cdr sxpr)))
			    ((list? sxpr) (cons (rex (car sxpr)) (rex (cdr sxpr))))
			    (else sxpr)))))
	(rex anot))))


;;HW3- part 7


(define hw3var-or-const?
    (lambda (sxpr) (and (list? sxpr) (< 0 (length sxpr)) (or (equal? 'var (car sxpr)) (equal? 'const (car sxpr))))))

   ;or

(define hw3if3?
    (lambda (sxpr) (and (list? sxpr) (< 0 (length sxpr)) (equal? 'if3 (car sxpr)))))

(define hw3seq?
    (lambda (sxpr) (and (list? sxpr) (< 0 (length sxpr)) (equal? 'seq (car sxpr)))))

(define hw3def?
    (lambda (sxpr) (and (list? sxpr) (< 0 (length sxpr)) (equal? 'def (car sxpr)))))

(define hw3lambda?
    (lambda (sxpr) (and (list? sxpr) (< 0 (length sxpr)) (or (equal? 'lambda-simple (car sxpr)) (equal? 'lambda-opt (car sxpr)) (equal? 'lambda-var (car sxpr))))))

(define hw3applic?
    (lambda (sxpr) (and (list? sxpr) (< 0 (length sxpr)) (equal? 'applic (car sxpr)))))





(define annotate-tc (lambda (sxpr)
            (letrec ((header (lambda (sxpr) (if (equal? 'lambda-opt (car sxpr)) (list (car sxpr) (cadr sxpr) (caddr sxpr)) (list (car sxpr) (cadr sxpr)))))
		    (tc-map-last (lambda (lst tp?)
                                        (cond ((null? lst) '())
                                              ((= 1 (length lst)) (list (anotate (car lst) tp?)))
                                              (else (cons (anotate (car lst) #f) (tc-map-last (cdr lst) tp?))))))

                        (anotate (lambda (sxpr tp?)
                            (cond ((not (list? sxpr)) sxpr)
                                  ((hw3var-or-const? sxpr) sxpr)
                                  ((or? sxpr) `(or ,(tc-map-last (cadr sxpr) tp?)))
                                  ((hw3seq? sxpr) `(seq ,(tc-map-last (cadr sxpr) tp?)))
                                  ((hw3if3? sxpr) `(if3 ,(anotate (cadr sxpr) #f)  ,(anotate (caddr sxpr) tp?)  ,(anotate (cadddr sxpr) tp?)))
                                  ((hw3def? sxpr) `(def ,(cadr sxpr) ,(anotate (caddr sxpr) tp?)))
                                  ((hw3lambda? sxpr) `(,@(header sxpr) ,(anotate (body-getter sxpr) #t)))
                                  ((and tp? (hw3applic? sxpr)) `(tc-applic ,(anotate (cadr sxpr) #f) ,(tc-map-last (caddr sxpr) #f)))
                                  ((hw3applic? sxpr) `(applic ,(anotate (cadr sxpr) #f) ,(tc-map-last (caddr sxpr) #f)))
                                   (else (map (lambda(e) (anotate e #f)) sxpr))))))
                        (anotate sxpr #f))))


;;proj

(define file->string
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
        (list->string
            (run))))))


(define code-gen-if (lambda (pe)
    `(,(code-gen (cadr pe))  (CMP R0 IMM(SOB_FALSE)) (JUMP_EQ L_if3_else_752) ,(code-gen (caddr pe)) (JUMP L_if3_exit_752) L_if3_else_752 ,(code-gen (cadddr pe)) L_if3_exit_752)))


(define code-gen-seq (lambda (pe)
    (map code-gen (cadr pe))))

(define code-gen-or
    (letrec ((rex (lambda (item) `(,(code-gen item) (CMP R0 IMM(SOB_FALSE)) (JUMP_NE(L_or_exit_497))))))
    (lambda (pe) `(,@(map rex (reverse (cdr (reverse (cadr pe))))) ,(code-gen (car (reverse (cadr pe)))) L_or_exit_497))))

(define code-gen (lambda (pe)
    (cond ((hw3if3? pe) (code-gen-if pe))
          ((hw3seq? pe) (code-gen-seq pe))
          ((or? pe) (code-gen-or pe))
        (else pe))))
