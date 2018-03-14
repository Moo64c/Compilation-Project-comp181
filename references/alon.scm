
(load "pc.scm")
(load "qq.scm")

; ################## HELPER PARSERS

(define char->symbol
  (lambda (ch)
    (string->symbol (string ch))
))

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <lineComment>
  (let ((<CommentsAfterNewline>
   (new (*parser (char #\newline))
        (*parser <end-of-input>)
        (*disj 2)
        done)))
    (new (*parser (char #\;))
   
   (*parser <any-char>)
   (*parser <CommentsAfterNewline>)
   *diff *star

   (*parser <CommentsAfterNewline>)
   (*caten 3)
   done)))

(define <skip>
  (lambda (<parser>)
  (new (*parser (word "#;"))
       (*delayed <parser>)
       (*caten 2)
       done)))

(define <comment>
  (lambda (<parser>)
    (new
      (*parser (<skip> <parser>))
      (*parser <lineComment>)
      (*disj 2)
      done)))


(define <endWithWhitespace>
  (lambda (<parser>)
    (new
      (*parser (<comment> <parser>))
      (*parser <whitespace>)
      (*disj 2)
  done)))

(define <remSpacesCommentsinfix>
    (lambda (<p>)
      (new (*parser (star (<endWithWhitespace> (lambda () <InfixExpression>))))
     (*parser <p>)
     (*parser (star (<endWithWhitespace> (lambda () <InfixExpression>))))
     (*caten 3)
     (*pack-with
      (lambda (_left e _right) e))
     done)))

(define <remSpacesCommentsSexpr>
    (lambda (<p>)
      (new (*parser (star (<endWithWhitespace> (lambda () <Sexpr>))))
     (*parser <p>)
     (*parser (star (<endWithWhitespace> (lambda () <Sexpr>))))
     (*caten 3)
     (*pack-with
      (lambda (_left e _right) e))
     done)))
     
(define <removeSpaces>
    (lambda (<p>) ;gets parser and cleans all the whitespaces
      (new 
        (*parser (star <whitespace>))
        (*parser <p>)
        (*parser (star <whitespace>))
        (*caten 3)
        (*pack-with
         (lambda (_left e _right) e))
     done)))

(define <CleanSpaces>
  (lambda( <parser>)
    (new
      (*parser <whitespace>) *star
      (*parser <p>)
      (*parser <whitespace>) *star
      (*caten 3)
      *pack-with (lambda (leftSpace expression rightSpace) expression))
      ))



; Helper functions
; #####################################################################
(define <sexprWithSpace>                                                    
  (new

    (*delayed (lambda () <Sexpr>))
    (*parser (char #\space)) *star
    (*caten 2)
    (*pack-with (lambda (Sexpr space) Sexpr )) 

    (*parser (char #\space)) *star
    (*delayed (lambda () <Sexpr>)) 
    (*caten 2)
    (*pack-with (lambda (space Sexpr) Sexpr)) 

    (*delayed (lambda () <Sexpr>))

    (*disj 3)

    done))


(define <ParserWithSpaces>  
  (lambda(<parser>)                                                  
    (new

      (*delayed (lambda () <parser>))
      (*parser (char #\space)) *star
      (*caten 2)
      (*pack-with (lambda (Sexpr space) Sexpr )) 

      (*parser (char #\space)) *star
      (*delayed (lambda () <parser>))
      (*caten 2)
      (*pack-with (lambda (space Sexpr) Sexpr)) 

      (*delayed (lambda () <parser>))

      (*disj 3)

      done)))

(define is-unicode
  (lambda (num) 
    (and
  (or (= num #x0) (> num #x0)) 
  (or (= num #x10FFFF) (< num #x10FFFF)))
))

(define list->number
  (lambda (base)
    (lambda (lst)
      (string->number (list->string lst) base))
  ))

(define list->hex-number (list->number 16))

(define list->decimal-number (list->number 10))

(define list->symbol
  (lambda (lst)
    (string->symbol (list->string lst))
))


;######################### CHAR ##########################

(define <HexChar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\f))
   (*disj 2)
  done))

(define <CharPrefix> (word "#\\"))

(define <NamedChar>
  (new
   (*parser (word-ci "lambda"))
   (*parser (word-ci "newline"))
   (*parser (word-ci "nul"))
   (*parser (word-ci "page"))
   (*parser (word-ci "return"))
   (*parser (word-ci "space"))
   (*parser (word-ci "tab"))
   (*disj 7)
   (*delayed (lambda () <Symbol>))
    *not-followed-by
   (*pack list->string)
done))

(define <VisibleSimpleChar>
 (const (lambda (ch) (char<=? (integer->char 33) ch))))


(define is-unicode
  (lambda (num) 
    (and
  (or (= num #x0) (> num #x0)) 
  (or (= num #x10FFFF) (< num #x10FFFF)))
))


(define <HexUnicodeChar> 
  (new
  (*parser (char-ci #\x))
  (*parser <HexChar>) *plus
  (*caten 2)

 (*pack-with (lambda (first rest)
   (list->hex-number `(,@rest))))
  (*only-if is-unicode)
  (*pack integer->char)  
done))


 (define <Char>
  (new
   (*parser <CharPrefix>)
   
   (*parser <VisibleSimpleChar>)
   (*parser <VisibleSimpleChar>)
   (*parser (char #\]))
   (*parser (char #\[))
   (*parser (char #\)))
   (*parser (char #\())
   (*disj 4)
   *diff
   *not-followed-by

   (*parser <NamedChar>)
   (*pack (lambda (namedChar) 
    ((lambda (str)
    (cond ((string-ci=? "lambda" str) (integer->char 955))
          ((string-ci=? "newline" str) #\newline)
          ((string-ci=? "nul" str) #\nul)
          ((string-ci=? "page" str) #\page)
          ((string-ci=? "return" str) #\return)
          ((string-ci=? "space" str) #\space)
          ((string-ci=? "tab" str) #\tab)
          (else #f))
    ) namedChar)))

   (*parser <HexUnicodeChar>)
   (*disj 3)

   (*caten 2)
   (*pack-with (lambda (prefix ch) ch))
  done)) 

; ##################### String #########################

(define <StringHexChar>
  (new
  (*parser (word-ci "\\x"))
  (*parser <HexChar>) *star
  (*parser (char #\;))
  (*caten 3)
  (*pack-with (lambda (x hexChar semicolon)
      (list->hex-number `(,@hexChar))))
  (*only-if is-unicode)
  (*pack integer->char)         
 done))

(define <StringMetaChar>
  (new
   (*parser (word-ci "\\\\")) ; \\
   (*parser (word-ci "\\\"")) ; \"
   (*parser (word-ci "\\t"))  ; \t
   (*parser (word-ci "\\f"))  ; \f
   (*parser (word-ci "\\n"))  ; \n
   (*parser (word-ci "\\r"))  ; \r
   (*disj 6)
   (*pack (lambda(meta-ch)
    (let ((str (list->string meta-ch)))
      (cond 
      ((string-ci=? "\\\\" str) #\\)
    ((string-ci=? "\\\"" str) #\")
    ((string-ci=? "\\t" str) #\tab)
    ((string-ci=? "\\f" str) #\page)
      ((string-ci=? "\\n" str) #\newline)
      ((string-ci=? "\\r" str) #\return)
      (else #f)))
    ))
done))


(define <StringLiteralChar>  
  (const (lambda(ch) (and (not(char=? #\\ ch)) (not(char=? #\" ch))))))

 (define <StringChar> 
  (new 
    (*parser <StringHexChar>)
    (*parser <StringMetaChar>)
    (*parser <StringLiteralChar>)
    (*disj 3)
  done))

(define <String>
  (new
   (*parser (char #\"))
   (*parser <StringChar>)
   (*parser (char #\"))
   *diff
   *star
   (*parser (char #\"))
   (*caten 3)
   (*pack-with (lambda (leftGershaym str rightGershaym)
(list->string str)))
done)) 



; ################## Symbol ##################
(define <SymbolChar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\z))
   (*pack char-downcase)
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
   (*disj 14)
done))

(define <Symbol> 
  (new 
   (*parser <SymbolChar>) *plus
   (*pack (lambda (lst)
 (list->symbol lst))) 
done))

;############## Boolean ####################
(define <Boolean> 
  (new
   (*parser (word-ci "#t"))
   (*pack (lambda (bool) #t))

   (*parser (word-ci "#f"))
   (*pack (lambda (bool) #f))

   (*disj 2)
done))

;############## NUMBER ####################
(define <Natural>
  (new
   (*parser (range #\0 #\9)) *plus
   (*pack list->decimal-number)
done))


(define <Integer>
  (new
   (*parser (char #\+))
   (*parser <Natural>)
   (*caten 2)
   (*pack-with (lambda (plus number) number))

   (*parser (char #\-))
   (*parser <Natural>)
   (*caten 2)
   (*pack-with (lambda (minus num) (- num)))

   (*parser <Natural>)

   (*disj 3)
done))

(define <Fraction>
  (new
   (*parser <Integer>)
   (*parser (char #\/))
   (*parser <Natural>)
   (*caten 3)

   (*pack-with (lambda (int backSlash num) (/ int num)))
  done))


(define <Number>
  (new
    (*parser <Fraction>)
    (*parser <Integer>)
    (*disj 2)
  done))


(define <NumberNotFollowedBySymbol>
  (new
   (*parser <Number>)
   (*delayed (lambda () <Symbol>))
   (*parser (range #\0 #\9))
   (*parser (char #\/))
   (*disj 2)
   *diff
   *not-followed-by
  done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; InfixExtension ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(define <InfixPrefixExtensionPrefix>
  (<removeSpaces> (disj (word "##") (word "#%"))))


(define <InfixSymbol>
  (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\z))
   (*pack char-downcase)
   (*parser (char #\!))
   (*parser (char #\$))
   (*parser (char #\_))
   (*parser (char #\=))
   (*parser (char #\<))
   (*parser (char #\>))
   (*parser (char #\?))
   (*disj 9)
   *plus
   (*pack list->symbol)
   done)) 
   
(define <AtomicInfixValue>
    (new   
      (*parser <Number>)
      (*parser <InfixSymbol>)
      (*parser (range #\0 #\9))
      *diff
      *not-followed-by
      
      (*parser <InfixSymbol>)
      
      (*delayed (lambda ()  <InfixSexprEscape>)) 
      
      (*disj 3)
    done))
    
(define <PowerSymbol>
  (new
    (*parser (<remSpacesCommentsinfix> (char #\^)))
    (*parser (<remSpacesCommentsinfix> (word "**")))  
    (*disj 2)  
    (*pack (lambda (powSymbol) 'expt))
   done))
  
(define <InfixParen>
  (new 
    (*parser (char #\())
    (*delayed (lambda () (<remSpacesCommentsinfix> ^<InfixExpression>)))
    (*parser (char #\)))
    (*caten 3)
    (*pack-with (lambda (p1 exp p2) exp))   
            
    (*parser (<remSpacesCommentsinfix> <AtomicInfixValue>))
    (*disj 2)
  done)) 

(define <InfixArgList>
  (new
    (*delayed (lambda () <InfixExpression>))
    (*parser (char #\,))
    (*delayed (lambda () <InfixExpression>))   
    (*caten 2)
    (*pack-with (lambda (psik infixExpression) infixExpression))
    *star
    (*caten 2)
    (*pack-with (lambda (infixExpression rest) `(,infixExpression ,@rest)))
    
    (*parser <epsilon>)
    (*disj 2)
   done))  
     
(define <InfixArrayFuncall>
  (new           
    (*parser <InfixParen>)
    
    (*parser (<remSpacesCommentsinfix> (char #\[)))
    (*delayed (lambda () <InfixExpression>))
    (*parser (<remSpacesCommentsinfix> (char #\])))
    (*caten 3)
    (*pack-with (lambda (op1 exp op2) (cons 'array exp)))
    
    (*parser (<removeSpaces> (char #\()))
    (*parser <InfixArgList>)
    (*parser (<removeSpaces> (char #\))))
    (*caten 3)
    (*pack-with (lambda (op1 args op2) (cons 'funcall args))) 
    
    (*disj 2)
    *star
    
    (*caten 2)
    (*pack-with (lambda (first rest)
    (if (null? rest)
    first
        `,(fold-left (lambda (x y) 
        (if (equal? (car y) 'funcall)
        `(,x ,@(cdr y))
        `(vector-ref ,x ,(cdr y)))) first rest))))
        
    (*parser <InfixParen>)
    (*disj 2)
  done))      

(define <InfixNeg>
    (new
      (*parser (<removeSpaces> <InfixArrayFuncall>))
      
      (*parser (char #\-))
      (*parser (<removeSpaces> <InfixArrayFuncall>))
      (*caten 2)
      (*pack-with (lambda (Minus expr) `(- ,expr)))
      
      (*disj 2)
    done))
  

   

  (define <InfixPow>
    (new                                        
      (*parser (<removeSpaces> <InfixNeg>))
      (*parser (<removeSpaces> <PowerSymbol>))
      (*caten 2)
      (*pack-with cons)
      *star
      
      (*parser (<removeSpaces> <InfixNeg>))       
      (*caten 2)

      (*pack-with (lambda (rest last) 
        (if (null? rest)
          last
          (if (equal? (length rest) 1) `(,(cdar rest) ,(caar rest) ,last)
        `,(fold-left (lambda (x y) `(,(cdr y) ,(car y) ,x)) last (reverse rest))))))
     done)) 

(define <AddSub>
  (new
    (*parser (<remSpacesCommentsinfix> (char #\+)))
    (*parser (<remSpacesCommentsinfix> (char #\-)))
    (*disj 2)
    (*pack char->symbol)    
   done))
   
(define <MultDiv>
  (new
    (*parser (<remSpacesCommentsinfix> (char #\*)))
    (*parser (<remSpacesCommentsinfix> (char #\/)))
    (*disj 2)
    (*pack char->symbol)
   done))  


(define <InfixMultDiv>
    (new
      (*parser (<removeSpaces> <InfixPow>))
      (*parser (<removeSpaces> <MultDiv>))
      (*parser (<removeSpaces> <InfixPow>))
      (*caten 3)
      (*pack-with (lambda (left sign right) `(,sign ,left ,right)))
        
      (*parser (<removeSpaces> <MultDiv>))      
      (*parser (<removeSpaces> <InfixPow>))
      (*caten 2)
      (*pack-with cons)
      *star
      
      (*caten 2)
      (*pack-with (lambda (first rest) 
      (if (null? rest)
      first
    `,(fold-left (lambda (x y) `(,(car y) ,x ,(cdr y))) first rest))))
      
      (*parser (<removeSpaces> <InfixPow>))
      (*disj 2)  
done))
  
  
(define <InfixAddSub>
    (new
      (*parser (<removeSpaces> <InfixMultDiv>))
      (*parser (<removeSpaces> <AddSub>))
      (*parser (<removeSpaces> <InfixMultDiv>))
      (*caten 3)
      (*pack-with (lambda (n1 sign n2) `(,sign ,n1 ,n2)))
        
      (*parser (<removeSpaces> <AddSub>))      
      (*parser (<removeSpaces> <InfixMultDiv>))
      (*caten 2)
      (*pack-with cons)
      *star
      
      (*caten 2)
      (*pack-with (lambda (n1 n2) 
  (if (null? n2)
    n1
  `,(fold-left (lambda (x y) `(,(car y) ,x ,(cdr y))) n1 n2))))
      
      (*parser (<removeSpaces> <InfixMultDiv>))
      (*disj 2)  
done))
     
  

(define ^<InfixExpression>
  (new
    (*parser (<removeSpaces> <InfixAddSub>))
  done))
  
  
(define <InfixSexprEscape>
  (new
    (*parser (<removeSpaces> <InfixPrefixExtensionPrefix>))
    (*delayed (lambda () (<remSpacesCommentsSexpr> <Sexpr>)))
    (*caten 2)
    (*pack-with (lambda (_ sexp) sexp))
  done))
    
(define <InfixExpression> (<remSpacesCommentsinfix> ^<InfixExpression>))
   
   
(define <InfixExtension>
  (new 
    (*parser <InfixPrefixExtensionPrefix>)  
    (*parser <InfixExpression>)
    (*caten 2)
    (*pack-with (lambda (prefix exp) exp))    
done))
     ;######## end of infix ###############################################
 

  (define <ProperList>
  (new
    (*parser (word "("))
    (*parser <sexprWithSpace>) *star
    (*parser (word ")"))
    (*caten 3)
    (*pack-with (lambda(left_br Sexpr right_br) Sexpr ))
    done)) 

     
(define <ImproperList>
  (new
    (*parser (word "("))
    (*parser <sexprWithSpace>) *plus
    (*parser (word ".") )
    (*delayed (lambda () <sexprWithSpace>))
    (*parser (word ")"))
    (*caten 5)
    (*pack-with (lambda(left_br sexpr1 dot sexpr2 right_br ) `(,@sexpr1 . ,sexpr2)))
    done)) 
  
     
(define <Vector>
  
  (new
   (*parser (word "#("))
   (*delayed (lambda () <sexpr>))
   *star
   (*parser (char #\)))   
   (*caten 3)
   (*pack-with (lambda (p1 expr p2)
     `#(,@expr)))
   done))

     
(define <Quoted>
  (new
    (*parser (word "'"))
    (*delayed (lambda () <Sexpr>))
    (*caten 2)
    (*pack-with (lambda(quot Sexpr) `'(,@Sexpr) ))
      done))
  
     
(define <QuasiQuoted>
  (new
    (*parser (word "`"))
    (*delayed (lambda () <Sexpr>))
    (*caten 2)
    (*pack-with (lambda(qq Sexpr)  (list 'quasiquote Sexpr)))
      done))

     
(define <Unquoted>
  (new
    (*parser (word ","))
    (*parser (word "@"))
    *not-followed-by
    (*delayed (lambda () <Sexpr>)) 
    (*caten 2)
    (*pack-with (lambda(unquot Sexpr) (list 'unquote Sexpr) ))
      done))
 
     
(define <UnquoteAndSpliced>
  (new
    (*parser (word ",@"))
    (*delayed (lambda () <Sexpr>)) 
    (*caten 2)
    (*pack-with (lambda(unquot Sexpr) (list 'unquote-splicing Sexpr)) )
      done)) 
     
     
(define <CBNameSyntax1>
  (new
    (*parser (word "@"))
    (*delayed (lambda () <Sexpr>))
    (*caten 2)
    (*pack-with (lambda(shtrudel Sexpr) (list 'cbname Sexpr)  ))
      done)) 
     
     
  (define <CBNameSyntax2>
  (new
    (*parser (word "{"))
    (*delayed (lambda () <Sexpr>))
    (*parser (word "}"))
    (*caten 3)
    (*pack-with (lambda(left_br Sexpr right_br) (list 'cbname Sexpr) ))
    done))  

     
(define <CBName>
  (disj <CBNameSyntax1> <CBNameSyntax2>))


(define <Sexpr> 
  (<remSpacesCommentsSexpr>
    (disj <Boolean>
          <Char>
          <NumberNotFollowedBySymbol>
          <String>
          <Symbol>
          <ProperList>
          <ImproperList>
          <Vector>
          <Quoted>
          <QuasiQuoted>
          <Unquoted>
          <UnquoteAndSpliced>
          <CBName>
          <InfixExtension>)))

(define <remSpacesCommentsSexpr> <remSpacesCommentsSexpr>)
(define <sexpr> <Sexpr>)

;############################## END OF assignment 1 #############################################################################################

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define handle-multi-begin
  (lambda (x)
    (cond ((null? x) '())
    ((and (list? (car x)) (equal? (caar x) 'seq))
     `(,@(cadar x) ,@(handle-multi-begin (cdr x))))
     (else `(,(car x) ,@(handle-multi-begin (cdr x)))))
))

(define handle-seq
  (lambda (exps)
    (if (equal? (length exps) 1)
  (parse (car exps))
  `(seq ,(handle-multi-begin (map parse exps)))))
)

(define flatten
  (lambda (x)
    (cond 
      ((null? x) '())
      ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
      (else (list x)))
))  

(define contains-duplicates?
  (lambda (lst)
    (not (andmap 
      (lambda (x) 
  (equal? 
    (- (length lst) 1) 
    (length (filter (lambda (y) (not (equal? x y))) 
        lst))
  )) 
    lst))
)) 

(define improper-list?
  (lambda (x) (and (pair? x) (not (list? x)))))

(define var?
  (lambda (var) (and (not (member var *reserved-words*)) (symbol? var))
))

(define check-simple-params?
(lambda (x) (and (list? x) (not (contains-duplicates? x)) (or (null? x) (and (list? x) (andmap var? x))))))


(define constant?
  (lambda (x) 
    (or 
      (and (symbol? x) (null? x)) 
      (vector? x) 
      (boolean? x) 
      (char? x) 
      (number? x) 
      (string? x) 
      (equal? (void) x)
    )
))

(define quote-const?
  (lambda (exp)
    (and  (pair? exp) (eq? (car exp) 'quote))))

(define if?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'if) (= (length exp) 4))))

(define if-without-else?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'if) (= (length exp) 3))))

(define or-without-args?
  (lambda (exp)
     (and (list? exp) (= (length exp) 1) (eq? (car exp) 'or))))

(define or-with-one-args?
  (lambda (exp)
   (and (list? exp) (= (length exp) 2) (eq? (car exp) 'or))))


(define or?
  (lambda (exp)
    (and (list? exp) (> (length exp) 2) (eq? (car exp) 'or))))

(define lambda-simple?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (check-simple-params? (cadr exp)))))

(define lambda-opt-im?
  (lambda (exp)
  (and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (or (improper-list? (cadr exp))))))

(define lambda-opt-var?
  (lambda (exp)
  (and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (var? (cadr exp)))))

(define lambda-var-improper?
  (lambda (exp)
  (and (list? exp) (eq? (car exp) 'lambda-im) (> (length exp) 2) (var? (cadr exp)))))

(define regular-define?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (var? (cadr exp)))))

(define mit-style-define?
  (lambda (exp)
  (and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (list? (cadr exp)))))

(define mit-style-improper-define-one-arg?
  (lambda(exp)
      (and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (improper-list? (cadr exp)) (= (length-improper-left (cadr exp)) 1))))

;same as before for now...
(define mit-style-improper-define-more-than-one-arg?
  (lambda(exp)
    (and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (improper-list? (cadr exp)))))

(define length-improper-left
  (lambda(im-lst)
    (if (pair? im-lst)
      (+ 1 (length-improper-left (cdr im-lst)))
      0 )))
      
(define set?
  (lambda (exp)
  (and (list? exp) (eq? (car exp) 'set!) (> (length exp) 2) (var? (cadr exp)))))

(define applic?
  (lambda (exp)
    (and (list? exp) (not (member (car exp) *reserved-words*)))))

(define let-without-bindings?
  (lambda (exp)
  (and (list? exp) (eq? (car exp) 'let) (> (length exp) 2) (null? (cadr exp)))))


(define let-with-bindings?
  (lambda (exp)
  (and (list? exp)  (eq? (car exp) 'let) (> (length exp) 2))))

(define let-star-without-bindings?
  (lambda (exp)
  (and (list? exp) (eq? (car exp) 'let*) (> (length exp) 2) (null? (cadr exp)))))

(define let-star-with-bindings?
  (lambda (exp)
  (and (list? exp) (eq? (car exp) 'let*) (> (length exp) 2))))

(define letrec-without-bindings?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'letrec) (> (length exp) 2) (null? (cadr exp)))))

(define letrec-with-bindings?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'letrec) (> (length exp) 2))))

(define and-without-args?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'and) (= (length exp) 1))))

(define and-with-one-arg?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'and) (= (length exp) 2))))

(define and?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'and) (> (length exp) 2))))

(define cond-with-one-exp?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'cond) (= (length exp) 2))))

(define cond?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'cond) (> (length exp) 2))))

(define cond-rec
  (lambda(lst-of-conds)
    (if (= (length lst-of-conds) 1)
      (if (eq? (caar lst-of-conds) 'else)
        `(begin ,@(cdar lst-of-conds))
        `(if ,(caar lst-of-conds) (begin ,@(cdar lst-of-conds))))
      `(if ,(caar lst-of-conds) (begin ,@(cdar lst-of-conds)) ,(cond-rec (cdr lst-of-conds))))))

(define else?
  (lambda(exp)
    (and (list? exp) (eq? (car exp) 'else) (= (length exp) 2))))

;get list and return the same list without the last element
(define get-all-but-last
  (lambda(lst)
    (if (> (length lst) 2)
      (cons (car lst) (get-all-but-last (cdr lst)))
      (cons (car lst) '()))))

(define begin-with-args?
  (lambda(exp)
    (and (list? exp) (eq? (car exp) 'begin) (> (length exp) 1))))

(define begin-without-args?
  (lambda(exp)
    (and (list? exp) (eq? (car exp) 'begin) (= (length exp) 1))))

(define quasiquote?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'quasiquote) (= (length exp) 2))))

(define parse
  (lambda (exp)
    (cond 
      ;const
      ((constant? exp) `(const ,exp))

      ;quote-const
      ((quote-const? exp) `(const ,@(cdr exp)))

        ;var
      ((var? exp) `(var ,exp))

        ;if
      ((if? exp)
        (let ((test (cadr exp))
            (then (caddr exp))
            (else (cadddr exp)))
            `(if3 ,(parse test) ,(parse then) ,(parse else))))

        ;if without else
      ((if-without-else? exp)
        (let ((test (cadr exp))
            (then (caddr exp)))
             `(if3 ,(parse test) ,(parse then) ,(parse (void)))))


        ;or-without-args
        ((or-without-args? exp) (parse #f))

        ;or-with-one-args
        ((or-with-one-args? exp) (parse (cadr exp)))
        ;or
        ((or? exp) `(or ,(map parse (cdr exp))))

        ;lambda-simple
        ((lambda-simple? exp)
          (let ((args (cadr exp))
                     (body (cddr exp)))
         `(lambda-simple ,args ,(handle-seq body))))

        ;lambda-opt
        ((lambda-opt-im? exp)
          (let* ((args-list (flatten (cadr exp)))
                 (rev-args-list (reverse args-list)))
        `(lambda-opt ,(reverse (cdr rev-args-list)) ,(car rev-args-list) ,(handle-seq (cddr exp)))))

        ; lambda-var
         ((lambda-opt-var? exp)
          (let ((arg (cadr exp))
              (body (cddr exp)))
          `(lambda-opt () ,arg ,(handle-seq body))))

         ; lambda-var-improper-more-than-one-arg
         ((lambda-var-improper? exp)
          (let ((arg (cadr exp))
              (body (cddr exp)))
          `(lambda-opt () ,arg  ,(handle-seq body))))

        ;regular-define
         ((regular-define? exp)
          (let ((var (cadr exp))
              (val (cddr exp)))
          `(define ,(parse var) ,(handle-seq val))))

         ;mit-style-define
         ((mit-style-define? exp)
          (let* ((var (car (cadr exp)))
               (args (cdr (cadr exp)))
               (body (cddr exp)))
          (parse `(define ,var ,`(lambda ,args ,@body)))))

         ;mit-style-improper-define
         ((mit-style-improper-define-one-arg? exp)
          (let* ((var (car (cadr exp)))
               (args (cdr (cadr exp)))
               (body (cddr exp)))
          (parse `(define ,var ,`(lambda-im ,args ,@body)))))

         ;mit-style-improper-define-more-than-one-arg?
         ((mit-style-improper-define-more-than-one-arg? exp)
          (let* ((var (car (cadr exp)))
               (args (cdr (cadr exp)))
               (body (cddr exp)))
          (parse `(define ,var ,`(lambda ,args ,@body)))))

         ;set!
         ((set? exp)
          (let ((var (cadr exp))
              (val (cddr exp)))
           `(set ,(parse var) ,(handle-seq val))))

         ;applic
         ((applic? exp)
          (let ((first-body (car exp))
              (rest-bodies (cdr exp)))
          `(applic ,(parse first-body) ,(map parse rest-bodies))))

         ;let-without-bindings
         ((let-without-bindings? exp)
          (let ((first-body (caddr exp))
              (rest-bodies (cdddr exp)))
          (parse `((lambda () ,first-body ,@rest-bodies)))))

         ;let-with-bindings
         ((let-with-bindings? exp)
          (let* ((bindings (cadr exp))
               (args-names (map car bindings))
               (args-values (map cadr bindings)))
            (parse `((lambda ,args-names ,(caddr exp) ,@(cdddr exp)) ,@args-values))))

         ;let-star-without-bindings
         ((let-star-without-bindings? exp)
          (let ((first-body (caddr exp))
              (rest-bodies (cdddr exp)))
          (parse `(let () ,first-body ,@rest-bodies))))

         ;let-star-with-bindings
         ((let-star-with-bindings? exp)
          (let ((handle-bindings (lambda (bindings body other-bodies)
          (if (null? bindings)
            `(,body ,@other-bodies)
            `((let* ,bindings ,body ,@other-bodies)))))
              (key (caaadr exp))
              (val (cdaadr exp))
              (other-bindings (cdadr exp))
              (body (caddr exp))
              (other-bodies (cdddr exp)))
        (parse `(let ((,key ,@val)) ,@(handle-bindings other-bindings body other-bodies)))))

         ;letrec-without-bindings
         ((letrec-without-bindings? exp)
          (let ((first-body (caddr exp))
              (rest-bodies (cdddr exp)))
          (parse `(let () (let () ,first-body  ,@rest-bodies)))))

         ;letrec-with-bindings
         ((letrec-with-bindings? exp)
          (let* ((key (caaadr exp))
              (val (cdaadr exp))
              (other-bindings (cdadr exp))
              (body (caddr exp))
              (other-bodies (cdddr exp))
              (args-names (append (list key) (map car other-bindings)))
              (set-args (map (lambda (x) `(set! ,(car x) ,(cadr x))) (append (list (list key (car val))) other-bindings))))
              (parse `((lambda ,args-names ,@set-args ,`(let () ,body ,@other-bodies)) ,@(make-list (length args-names) #f)))))


         ;and-without-args
         ((and-without-args? exp) (parse #t))

         ;and-with-one-arg
         ((and-with-one-arg? exp) (parse (cadr exp)))

         ;and
         ((and? exp)
         (let ((first (cadr exp))
            (rest (cddr exp)))
         `(if3 ,(parse first) ,(parse `(and ,@rest)) ,(parse #f))))

         ((and? exp)
         (let ((first (cadr exp))
            (rest (cddr exp)))
         `(if3 ,(parse first) ,(parse `(and ,@rest)) ,(parse #f))))

         ;cond-with-one-exp
         ((cond-with-one-exp? exp)
            (let ((cond (caadr exp))
                (body (cdadr exp)))
            (if (equal? (caadr exp) 'else)
              (handle-seq (cdadr exp))
          `(if3 ,(parse cond) ,(handle-seq body) ,(parse (void))))))

            ((begin-with-args? exp)
              (handle-seq (cdr exp)))

            ((begin-without-args? exp)
              (parse (void)))

            ((cond? exp) 
              (parse (cond-rec (cdr exp))))

          ;quasi-quote
          ((quasiquote? exp)
            (parse (expand-qq (cadr exp))))

      (else "ERROR: no such expression\n")))
)


;############################## END OF assignment 2 #############################################################################################





;;;;;;;;;;;;;;;;;;;;;; helper function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;return arguments of lambda as a list
(define lambda-args->lst
  (lambda (el)
    (let (  (lambda-type (car el))
        (args (cadr el)))
    (cond   ((equal? lambda-type 'lambda-simple) args)
          ((equal? lambda-type 'lambda-opt) (append args (list (caddr el))))
          (else (list args))))))


(define getLambdaArgs
  (lambda (el)
    (let ((lambda-type (car el))
    (args (cadr el)))
    (cond
      ((lambdaWithoutArgs? el) `(() ,(caddr el)))
      ((null? args) '(()))  
      ((equal? lambda-type 'lambda-simple) `(,args))
      ((equal? lambda-type 'lambda-opt) `(,args ,(caddr el)))
      (else (list args))))))



;returns #t if lambda-simple or lambda-opt
(define lambda?
    (lambda (expr)
        (and (list? expr) (not (null? expr))
         (or  (eq? (car expr) 'lambda-simple)
              (eq? (car expr) 'lambda-opt))))) 

;returns body according to lambda type
(define getLambdaBody
    (lambda (expr)
      (let ((lambdaType (car expr)))
        (if (equal? lambdaType 'lambda-simple) 
          (caddr expr)
          (cadddr expr)))))

(define setLambdaBody
    (lambda (orgLambda newBody)
        (cond ((eq? (car orgLambda) 'lambda-simple) `(lambda-simple ,(cadr orgLambda) ,newBody))
              ((eq? (car orgLambda) 'lambda-opt) `(lambda-opt ,(cadr orgLambda) ,(caddr orgLambda) ,newBody))
              (else #f))))


;returns args according to lambda type
(define getLambdaParameters
    (lambda (expr)
      (let ((lambdaType (car expr)))
        (if (eq? lambdaType 'lambda-simple)
          (cadr expr)
          (append (cadr expr) (list (caddr expr)))))))


(define var-exp?
  (lambda (expr)
    (member (car expr) '(var fvar bvar pvar const))))

(define lambdaWithoutArgs?
  (lambda (expr)
    (and (equal? (car expr) 'lambda-opt) (null? (cadr expr)))))



; ;;;;;;;;;;;;;;;;;;;;;; applicLambdaSimpleNil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define applicLambdaSimpleNil?
  (lambda (expr)
    (if (and (> (length expr) 1) (list? expr) (eq? (car expr) 'applic) (list? (cadr expr))) ;if applic tag
      (let ((expr (cadr expr)))
         (if (and (> (length expr) 2) (eq? (car expr) 'lambda-simple) (null? (cadr expr)))   
         (caddr expr)
         '()))
      '())))


(define removeApplicLambdaTag
  (lambda (expr)
      (let ((body (applicLambdaSimpleNil? expr)))
        (cond   ((null? body) expr)
            ((list? body) (remove-applic-lambda-nil body))
            (else body)))))

(define remove-applic-lambda-nil
  (lambda (lst)
    (map (lambda (el) (if (list? el) ;lambda
              (remove-applic-lambda-nil el)
              el))
    (removeApplicLambdaTag lst)))) ;list

;;;;;;;;;;;;;;;;;;;;;;;;;; box-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          

;eliminate-nested-defines-seq-pairs
; returns a list of defines, actualValues and the rest of the body
(define seqPairs
    (lambda (expr defines actualValues)
        (cond ((or (null? expr) (not (list? expr))) (list defines actualValues expr)) ; if expr isn't list nor null 
              ((equal? (caar expr) 'define) ;if define expression
                (seqPairs (cdr expr)
                             (append defines (list (cadar expr)))
                             (append actualValues (list (suffix (caddar expr))))))
              (else (list defines actualValues expr)))))


;called only with lambda body
(define suffixLambdaBody
    (lambda (expr)
      (let ((tag (car expr)))
          (cond ((or (not (list? expr)) (null? expr)) expr) ; (= (length expr) 1)
                ((and (equal? tag 'seq) (not (equal? (length (car (seqPairs (cadr expr) '() '()))) 0)))  ;its a sequence, there aren't any nested defines
                   (let* ((esp (seqPairs (cadr expr) '() '())) ; esp[0]=defs, esp[1]=actualvalueues, esp[2]=rest
                        (args (car esp))) 
                        `(applic (lambda-simple 
                                 ,(map cadr args)
                                 (seq ,(append (map (lambda (define value) `(set ,define ,value)) args (suffix (cadr esp))) (suffix (caddr esp)))))
                                ,(map (lambda (x) '(const #f)) args))))
                ((equal? tag 'define) ; tag is define
                  (let* ((esp (seqPairs (list expr) '() '()))) ; esp[0]=defs, esp[1]=actualvalueues, esp[2]=rest
                      `(applic (lambda-simple 
                                  ,(map cadr (car esp))
                                  (seq ,(list (map (lambda (define value) `(set ,define ,value)) (car esp) (suffix (cadr esp)))
                                              (suffix (caddr esp)))))
                               ,(map (lambda (x) '(const #f)) (car esp)))))
                (else (suffix expr))))))
        
              
;eliminate-nested-defines body
(define eliminate-nested-defines 
    (lambda (expr)
        (cond ((or (null? expr) (not (list? expr))) expr)
              ((equal? (car expr) 'define)
                `(define ,(cadr expr) ,(suffix (caddr expr))))
              (else (suffix expr)))))

(define suffix
    (lambda (expr) (if  (or (null? expr) (not (list? expr)))
              expr
              (if (lambda? expr)
                (setLambdaBody expr (suffixLambdaBody (getLambdaBody expr)))
                (map suffix expr)))))
 
 (define tagged?
    (lambda (expr)
        (and (list? expr)
             (not (null? expr)) 
             (eq? (car expr) 'set) 
             (list? (cadr expr)) 
             (= (length (cadr expr)) 2) 
             (eq? (caadr expr) 'var))))

(define prefixPush
    (lambda (expr toPush)
      (if (or (null? expr) (not (list? expr)))
        toPush
        (if (eq? (car expr) 'seq)
          `(seq ,(append (list toPush) (cadr expr)))
          `(seq (,toPush ,expr))))))

(define equalVar?
    (lambda (el var)
        (and (list? el) (equal? (length el) 2) (equal? (car el) 'var) (equal? (cadr el) var))))
              
; var is '<char>
(define varInParams?
    (lambda (expr var)                                                         ;the list of parameters
        (and (lambda? expr) (not (< (length (filter (lambda (param) (equal? param var)) (getLambdaParameters expr))) 1)))))
            
; var is 'a or 'b or so..
(define varNotInParams?
    (lambda (expr var)                                                    ;the list of parameters
        (and (lambda? expr) (< (length (filter (lambda (param) (equal? param var)) (getLambdaParameters expr))) 1))))

; var is 'a or 'b or so..
(define hasParamsOrIsSet?
    (lambda (expr var)
        (or (varInParams? expr var) (tagged? expr))))

; is expr fulfilling f untill there is lambda with this var inside of it which it ignores it and continue
; var     is 'a or 'b or so..
; proc must receive an expr as a list and a value and checks if the list fullfills it needs
; suffixFunc must receive an expr as a list and a value and cut the search in that branch if it fullfills it needs
(define sameUntilSuffixFunc
    (lambda (proc var suffixFunc)
        (lambda (expr)
          (if (or (not (list? expr)) (null? expr) (suffixFunc expr var))
            #f
            (if (proc expr var)
              #t
              (ormap (sameUntilSuffixFunc proc var suffixFunc) expr))))))
               
; find if under an expr tree there is a var with that name (until it get to a leaf or a lambda with redeclaration of that var)
(define appearsInExp?
    (lambda (expr var) ((sameUntilSuffixFunc equalVar? var varInParams?) expr)))
                
; checks if the variable is bounded
; if there is a lambda in the expr (flatly - not as tree search), it search this lambda fully to find if the variable is called there
(define Bounded?
    (lambda (expr var)
        (let ((NotInParamsAndAppearsInExp (lambda (nestedExp) (and (varNotInParams? nestedExp var) (appearsInExp? (getLambdaBody nestedExp) var)))))
             (or (NotInParamsAndAppearsInExp expr) (ormap NotInParamsAndAppearsInExp expr)))))
            
            
; checks flatly whether there is a set in the expr
(define taggedVar?
    (lambda (expr var) (and (tagged? expr) (equal? (cadadr expr) var))))

             
; find if under an expr tree there is a var with that name (untill it get to a leaf or a lambda with redeclaration of that var)
(define appearsInExp?
    (lambda (expr var)
        ((sameUntilSuffixFunc equalVar? var varInParams?)
         expr)))             
             



; checks if the variable is called (excluding (set var to something))
; if there is a set in the expr (flatly - not as tree search), it search this set fully by himself to find if the variable is called there
(define varCalled?
    (lambda (expr var)
        (let ((teggedAndsameUntilSuffixFunc (lambda (nestedExp)
                                        (and (tagged? nestedExp) ((sameUntilSuffixFunc varCalled? var hasParamsOrIsSet?) (caddr nestedExp))))))
          (or (equalVar? expr var) (ormap teggedAndsameUntilSuffixFunc expr)))))
                
; search if all three of the requirments are fulfilled in the expr
(define box-set?
    (lambda (expr var)
        (and ((sameUntilSuffixFunc Bounded? var varInParams?) expr)
             ((sameUntilSuffixFunc taggedVar? var varInParams?) expr)
             ((sameUntilSuffixFunc varCalled? var hasParamsOrIsSet?) expr))))
     
(define BoxSetGet
    (lambda (var)
        (lambda (expr)
            (cond ((or (null? expr) (not (list? expr)) (varInParams? expr var)) expr)
                ((equalVar? expr var) `(box-get (var ,var)))
                ((taggedVar? expr var) `(box-set (var ,var) ,((BoxSetGet var) (caddr expr))))
                (else (map (BoxSetGet var) expr))))))
                
; checks if a variable is need to be replaced and if so, replaces it.
(define lambdaBox
    (lambda (expr var)
      (cond ((box-set? expr var) (prefixPush ((BoxSetGet var) expr) `(set (var ,var) (box (var ,var)))))
             (else expr))))

; replaces lst variables if needed inside expression
(define lambdaBoxList
    (lambda (expr lst)
      (cond   ((null? lst) expr)
          (else (lambdaBoxList (lambdaBox expr (car lst)) (cdr lst))))))

(define box-set
    (lambda (expr)
        (cond ((or (not (list? expr)) (null? expr)) expr)
              ((lambda? expr) (setLambdaBody expr 
                                            (box-set (lambdaBoxList (getLambdaBody expr)
                                                                            (reverse (getLambdaParameters expr))))))
              (else (map box-set expr)))))



;;;;;;;;;;;;;;;;;;;;;; pe->lex-pe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define replaceBound
  (lambda (var major minor expr)
      (cond ((null? expr) expr)
      ((list? expr) (cond ((and (eq? 2 (length expr)) (equal? (car expr) 'var) (equal? (cadr expr) var)) `(bvar ,var ,major ,minor))
                    ((and (lambda? expr) (not (member var (lambda-args->lst expr)))) `(,(car expr) ,@(getLambdaArgs expr) ,(replaceBound var (+ 1 major) minor (getLambdaBody expr))))
                    ((lambda? expr) expr)
                    (else (map (lambda (x) (replaceBound var major minor x)) expr))))
    (else expr))))  

(define replaceParam
  (lambda (var minor expr)
        (cond ((null? expr) expr)
      ((list? expr) (cond ((and (equal? 2 (length expr)) (equal? (car expr) 'var) (equal? (cadr expr) var)) `(pvar ,var ,minor))
                    ((and (lambda? expr) (not (member var (lambda-args->lst expr)))) `(,(car expr) ,@(getLambdaArgs expr) ,(replaceBound var 0 minor (getLambdaBody expr)))) ; if bounded
                    ((lambda? expr) expr)     
                    (else (map (lambda (x) (replaceParam var minor x)) expr))))
    (else expr))))  

(define replaceParams
  (lambda (vars minor expr)
    (if (null? vars) 
      expr
    (replaceParams (cdr vars) (+ minor 1) (replaceParam (car vars) (+ minor 1) expr)))))


(define SearchParamBoundedVars
    (lambda (expr)
      (if (null? expr)
        expr
        (if (list? expr)
          (if (and (not (null? expr)) (lambda?  expr))
            `(,(car expr) ,@(getLambdaArgs expr),(SearchParamBoundedVars (replaceParams (lambda-args->lst expr) -1  (getLambdaBody expr))))
            (map SearchParamBoundedVars expr))
          expr))))

(define pe->lex-pe
    (lambda (expr)
      (letrec ( (ParamBoundedVars (SearchParamBoundedVars expr))
            (iter (lambda (ParamBoundedVars) 
              (cond   ((null? ParamBoundedVars) ParamBoundedVars) 
                  ((list? ParamBoundedVars) 
                    (if (and (equal? (length ParamBoundedVars) 2) (equal? (car ParamBoundedVars) 'var))
                      `(fvar ,(cadr ParamBoundedVars))
                   (map iter ParamBoundedVars)))
                  (else ParamBoundedVars)))))
        (iter ParamBoundedVars))))


;;;;;;;;;;;;;;;;;;;;;; annotate-tc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define anotate-tc-run
  (lambda (expr in-tp)
      (cond ((null? expr) expr)
      ((list? expr) (cond ((lambda? expr) `(,(car expr) ,@(getLambdaArgs expr) ,(anotate-tc-run (getLambdaBody expr) #t)))
                    ((equal? (car expr) 'applic) (if  in-tp 
                                    `(tc-applic ,@(anotate-tc-run (cdr expr) #f))
                                    `(applic ,@(anotate-tc-run (cdr expr) #f))))  
                    ((equal? (car expr) 'if3) (let ((test (cadr expr))
                                      (dit (caddr expr))
                                      (dif (cadddr expr)))
                              `(if3 ,(anotate-tc-run test #f) ,(anotate-tc-run dit in-tp) ,(anotate-tc-run dif in-tp))))
                    ((member (car expr) '(set box-set)) `(,(car expr) ,(cadr expr) ,(anotate-tc-run (caddr expr) #f)))
                    ((var-exp? expr) expr)
                    ((or (equal? (car expr) 'seq) (equal? (car expr) 'or)) (let* ( (operator (car expr))
                                                 (reversedList (reverse (cadr expr)))
                                                   (last (car reversedList))
                                                   (withoutLast (reverse (cdr reversedList))))
                                            `(,operator (,@(map (lambda (subExp) (anotate-tc-run subExp #f)) withoutLast) ,(anotate-tc-run last in-tp)))))
                    (else (map (lambda (subExp) (anotate-tc-run subExp in-tp)) expr))))
      (else expr))))

(define annotate-tc
  (lambda (expr)
    (anotate-tc-run expr #f)))



;############################## END OF assignment 3 #############################################################################################

;############################## final project #############################################################################################

(define next-free-ind-in-mem 1)

(define primitive-zero-pred
  (lambda (fvars)
    (string-append 
    "\njmp LmakeZeroPredClos
     LZeroPredBody: 
      push rbp
      mov rbp, rsp
      mov rcx,qword [rbp + 3*8]
      cmp rcx,2
      jne L_cannot_apply_non_closure
      
      mov rcx,qword [rbp + 4*8]    
      
      ; CMP(INDD(R1,0),IMM(T_FRACTION)); 
      ; JUMP_EQ(L_not_zero_zero_pred_body); 
      
      mov rax,rcx
      TYPE rcx
      cmp rcx, T_INTEGER
      jne(L_cannot_apply_non_closure); 
      
      cmp rax,MAKE_LITERAL(T_INTEGER,0)
      jne L_not_zero_zero_pred_body
      
      mov rax,SOB_TRUE
      jmp L_zero_pred_exit
      
      L_not_zero_zero_pred_body:
      mov rax,SOB_FALSE
      
      L_zero_pred_exit:
      leave
      ret
            
      LmakeZeroPredClos: 
      mov rax,"(search-addr 'zero? fvars)
      "\n
      MAKE_LITERAL_CLOSURE rax, rcx, LZeroPredBody
      \n\n"
)))

;takes all these functions and creates labels. we print these label. we need to changed the name of asaf-lior
(define primitive-functions-list
  '(car cdr apply cons 
    null? boolean? char? integer? pair? number? zero? procedure? symbol? vector?
    char->integer integer->char
    string-length vector-length make-string string? string-ref string-set!
    make-vector vector-ref vector-set!
    denominator numerator remainder
    set-car! set-cdr!
    symbol->string string->symbol
    eq?    
    asaf-lior-list-to-vector 
    asaf-lior-reduce-num asaf-lior-opposite-num asaf-lior-inverse-num
    asaf-lior-binary-int-frac-plus asaf-lior-binary-int-int-plus asaf-lior-binary-frac-frac-plus
    asaf-lior-binary-int-int-mul asaf-lior-binary-int-frac-mul asaf-lior-binary-frac-frac-mul
    asaf-lior-greater-than-int-int)
)

;another implementation to the procedure from the lecture list-to-set ???
(define remove-duplicates
  (lambda (lst)
    (if (null? lst) 
    	lst 
    	(cons 	(car lst) 
    			(remove-duplicates 
      				(filter (lambda (y) (not (equal? (car lst) y))) lst))))))

;returns the address of the free-vars table
(define fvar-addr
  (lambda (lst addr)
    (if (null? lst)
      (begin (set! next-free-ind-in-mem addr) lst)
      `((,(car lst) ,(string-append "LGLOB" (number->string addr))) ,@(fvar-addr (cdr lst) (+ addr 1))))))

;returns the address of the const table
(define ct-addr
  (lambda (lst counter)
    (let ((label-name (string-append "LCONST" (number->string counter))))
    (cond
      ((null? lst) (begin (set! next-free-ind-in-mem counter) '()))
      ((list? lst)
  (cond 
    ((equal? (car lst) (void)) `((,(car lst) ,label-name) ,@(ct-addr (cdr lst) (+ 1 counter))))
    ((null? (car lst)) `((,(car lst) ,label-name) ,@(ct-addr (cdr lst) (+ 1 counter))))
    ((equal? #t (car lst)) `((,(car lst) ,label-name) ,@(ct-addr (cdr lst) (+ 2 counter))))
    ((boolean? (car lst)) `((,(car lst) ,label-name) ,@(ct-addr (cdr lst) (+ 2 counter))))
    ((number? (car lst)) 
      (if (integer? (car lst))
        `((,(car lst) ,label-name) ,@(ct-addr (cdr lst) (+ 2 counter))) 
        `((,(car lst) ,label-name) ,@(ct-addr (cdr lst) (+ 3 counter)))))
    ((pair? (car lst)) `((,(car lst) ,label-name) ,@(ct-addr (cdr lst) (+ 3 counter))))
    ((char? (car lst)) `((,(car lst) ,label-name) ,@(ct-addr (cdr lst) (+ 2 counter))))

    ((vector? (car lst)) `((,(car lst) ,label-name) ,@(ct-addr (cdr lst) 
        (+ 2 (length (vector->list (car lst))) counter))))
    ((string? (car lst)) `((,(car lst) ,label-name) 
          ,@(ct-addr (cdr lst) (+ 2 (string-length (car lst)) counter))))
    ((symbol? (car lst)) `((,(car lst) ,label-name) 
          ,@(ct-addr (cdr lst) 
          (+ 2 counter))))          
    (else (error 'ct-addr "PANIC"))))
      (else (error 'ct-addr "PANIC")))
)))

(define create-fvar-table
  	(lambda (exps start-addr)
   		(letrec ( 	(iter (lambda (exps) 
    					(cond 	((null? exps) '())
    							((list? exps)
      								(cond 	((equal? 'fvar (car exps))
    											(list (cadr exps)))
        									(else (append 	(iter (car exps))
          													(iter (cdr exps))))))
    							(else '())))))
    		(fvar-addr (remove-duplicates `(,@primitive-functions-list ,@(iter exps))) start-addr)
)))

;in PAIR expression, we need to create sub-pairs( '(1 2 3) -> '1 and '(2 3))
; we create a label for each constant
(define create-sub-constants
  (lambda (e)
    (cond ((pair? e)
      `(,@(create-sub-constants (car e))
        ,@(create-sub-constants (cdr e))
        ,e))
    ; ((symbol? e)
    ;   `(,@(create-sub-constants (symbol->string e)) ,e))
    ((vector? e)
      `(,@(apply append (map create-sub-constants (vector->list e))) ,e))
    (else `(,e)))
))

#| (define create-constants-table
	(lambda (exps)
    	(letrec ((iter 
    		(lambda (exps) 
    			(cond 	((null? exps) '())
    					((list? exps) (cond ((equal? 'const (car exps))
    											(list (cadr exps)))
        									(else (append (iter (car exps))
          (iter (cdr exps))))))
    (else '())))))
    (ct-addr `(,(void) () #t #f
    ,@(filter 
      (lambda (x) (not (or (null? x) (boolean? x) (equal? x (void)))))
    (remove-duplicates (apply append (map create-sub-constants
      (remove-duplicates 
      (iter exps)))))
      )) 1)
))) |#

;takes the parsed program and apply the constants on them
(define create-constants-table
	(lambda (exps)
    	(letrec ((iter 
    		(lambda (exps) 
    			(cond 	((null? exps) '())
    					((list? exps) 
    						(if (equal? 'const (car exps))
    							(list (cadr exps))
    							(append (iter (car exps))
    									(iter (cdr exps)))))
    					(else '())))))
    	  (ct-addr `(,(void) () #t #f
    ,@(filter 
      	(lambda (x) (not (or (null? x) (boolean? x) (equal? x (void)))))
    	(remove-duplicates (apply append (map create-sub-constants
      		(remove-duplicates 
      			(iter exps)))))
      )) 1)
))) 

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
  (list->string (run))))
))

;don't need to understand this
(define file->list
  	(lambda (filename)
  		(let 	((input (open-input-file filename)))
  			(letrec ((run (lambda ()
  					(let ((e (read input)))
  						(if (eof-object? e)
  							(begin (close-input-port input) '())
  							(cons e (run)))))))
  				(run)))))

;we read the input and parse it with the first 3 assignment
(define list->sexprs
  (lambda (lst)
    (if (null? lst)
    	lst
  		(<sexpr> lst
    		(lambda (x y) `(,x ,@(list->sexprs y)))
    		(lambda (x) `(Failed to parse sexpr ,x))))))

; used in constant-symbol procedure
(define search-pair
  	(lambda (el lst)
    	(if (null? lst) 
    		(error 'search-pair "PANIC")
      		(if (equal? el (caar lst))
    			(car lst)
    			(search-pair el (cdr lst))))))

;people were not sure how to use it, so Guy left it for now
(define constant-symbols 
  	(lambda (lst orig-lst)
    	(display lst)
    	(if (null? lst)
    	 	lst
  			(if (symbol? (caar lst))   
    			`(,(search-pair (symbol->string (caar lst)) orig-lst) ,@(constant-symbols (cdr lst) orig-lst))
    			`(,@(constant-symbols (cdr lst) orig-lst))))))

;at the beginning, attach SOB_UNDEFINED to each free var. in runtime(?) we will update it to the real value
(define fvar-table->assembly
  	(lambda (lst)
    	(if (null? lst) 
    		""
  			(apply string-append 
  				(map 
    				(lambda (x) (string-append (cadr x) ":\n dq SOB_UNDEFINED\n")) 
    				lst)))))


;appears in the begging of every function and makes it string?
(define list-of-exps->str
  	(lambda (exps)
    	(if (null? exps) 
    		""
  			(string-append 
    			(if (list? (car exps)) 
      				(list-of-exps->str (car exps))
      				(cond ( (number? (car exps)) (number->string (car exps)))
    						((symbol? (car exps)) (symbol->string (car exps)))
    					; 	((boolean? (car exps)) "BOOL")
    						(else (car exps))))
      			; (format "\n")
    			(list-of-exps->str (cdr exps))))))

;gets list of f-vars or list of const and returns the label 
;when running in Assembly, we can figure out where the value of what we're looking for is located.
(define search-addr
  	(lambda (el lst)
    	(if (null? lst) 
    		(error 'search-addr "PANIC")
      		(if (equal? el (caar lst))
    			(cadar lst)
    			(search-addr el (cdr lst))))))

; each label is created with the data structures of Mayer
(define make-const-label
  	(lambda (name value)
    	(string-append name ":\ndq " value)))

(define constants-table->assembly
   	(lambda (lst)
     	(list-of-exps->str 
     		(map (lambda (x)
     			(cond 	((null? (car x)) (make-const-label (cadr x) "SOB_NIL\n"))
  						((equal? (car x) (void)) (make-const-label (cadr x) "SOB_VOID\n"))
  						((boolean? (car x))
    						(make-const-label (cadr x) (if (equal? (car x) #f) "SOB_FALSE\n" "SOB_TRUE\n")))
  						((number? (car x))
    						(if (integer? (car x))
    							(make-const-label (cadr x) (string-append "MAKE_LITERAL(T_INTEGER," (number->string (car x)) ")\n"))))

  ;####################### fraction ##########################3     
      ; (string-append
      ; "PUSH(IMM(" (number->string (denominator  (car x))) "));\n"
      ; "PUSH(IMM(" (number->string (numerator (car x))) "));\n"
      ; "CALL(MAKE_SOB_FRACTION);
      ;  DROP(2);\n"
      ;    )))
  ((pair? (car x))
      (make-const-label (cadr x) (string-append
       "MAKE_LITERAL_PAIR(" (search-addr (caar x) lst) "," (search-addr (cdar x) lst) ")\n" )))

    ; (string-append 
    ;   "PUSH(IMM(" (number->string (search-addr (cdar x) lst)) "));\n"
    ;   "PUSH(IMM(" (number->string (search-addr (caar x) lst)) "));\n"
    ;   "CALL(MAKE_SOB_PAIR);
    ;    DROP(2);\n"))
  ((char? (car x))
    (make-const-label (cadr x) (string-append "MAKE_LITERAL(T_CHAR,"
     (number->string (char->integer (car x))) ")\n")))
    ; (string-append
    ;   "PUSH(IMM(" (number->string (char->integer (car x))) "));
    ;    CALL(MAKE_SOB_CHAR);
    ;    DROP(1);\n"))
  ; ##########################((symbol? (car x))######################### 
    ((symbol? (car x)) "SYMBOL - NEED TO IMPLEMENT\n")

  ;   (string-append
  ;     "PUSH(IMM(" (number->string (search-addr (symbol->string (car x)) lst)) "));
  ;      CALL(MAKE_SOB_SYMBOL);
  ;      DROP(1);\n"))       
  ((string? (car x))
     (make-const-label (cadr x) (string-append "MAKE_LITERAL_STRING \"" (car x) "\"\n")))

    ; (string-append
    ;   (list-of-exps->str
    ;     (map 
    ; (lambda (x)
    ;   (string-append "PUSH(IMM(" (get-ascii x) "));\n")) 
    ;     (string->list (car x))))
    ;     "PUSH(IMM(" (number->string (string-length (car x))) "));\n"
    ;     "CALL(MAKE_SOB_STRING);\n
    ;      DROP(" (number->string (+ 1 (string-length (car x)))) ");\n"
    ;     ))
   ((vector? (car x))
      (let ((v-l (vector->list (car x))))
        (make-const-label (cadr x) (string-append "MAKE_LITERAL_VECTOR "
            (string-append (search-addr (car v-l) lst)
              (list-of-exps->str 
            (map 
              (lambda (x)
               (string-append "," (search-addr x lst))) (cdr v-l))))
            "\n")))) 


    ; (let ((v-l (vector->list (car x))))
    ;   (string-append
    ;     "// VECTOR - START \n"
    ;     (list-of-exps->str
    ; (map 
    ;   (lambda (x)
    ;     (string-append "PUSH(IMM(" (number->string (search-addr x lst)) "));\n")) 
    ; v-l))
    ; "PUSH(IMM(" (number->string (length v-l)) "));\n"
    ; "CALL(MAKE_SOB_VECTOR);\n
    ; DROP(" (number->string (+ 1 (length v-l))) ");\n"
    ; "// VECTOR - END \n")))    
   (else (error 'constants-table->assembly "PANIC")))) lst))
))


(define pe?
  (lambda (type)
    (lambda (pe)
      (equal? type (car pe)))))


;we use these definitions in the code-gen
(define pe-const? (pe? 'const))
(define pe-pvar? (pe? 'pvar))
(define pe-bvar? (pe? 'bvar))
(define pe-fvar? (pe? 'fvar))
(define pe-def? (pe? 'define))
(define pe-if3? (pe? 'if3))
(define pe-or? (pe? 'or))
(define pe-seq? (pe? 'seq))
(define pe-set? (pe? 'set))
(define pe-box? (pe? 'box))
(define pe-box-set? (pe? 'box-set))
(define pe-box-get? (pe? 'box-get))
(define pe-applic? (pe? 'applic))
(define pe-tc-applic? (pe? 'tc-applic))
(define pe-lambda-simple? (pe? 'lambda-simple))
(define pe-lambda-var? (pe? 'lambda-var))
(define pe-lambda-opt? (pe? 'lambda-opt))


(define label-counter 0)

;create a label L_Const
(define create-label
	(lambda (str)
		(lambda ()
      		(let ((counter (number->string label-counter)))
  				(set! label-counter (+ 1 label-counter))
  				(string-append str "_" counter)))))

;these lables concatonate a number to the end of the label in order to prevent lables with the same name
(define label-if3-else (create-label "L_if3_else"))
(define label-if3-exit (create-label "L_if3_exit"))
(define label-or (create-label "L_or"))
(define label-or-exit (create-label "L_or_exit"))
(define label-closure-body (create-label "L_clos_body"))
(define label-closure-exit (create-label "L_clos_exit"))
(define label-copy-params-loop (create-label "L_copy_params_loop"))
(define label-copy-params-exit (create-label "L_copy_params_exit"))
(define label-copy-prev-env-loop (create-label "L_copy_prev_env_loop"))
(define label-copy-prev-env-exit (create-label "L_copy_prev_env_exit"))
(define label-fix-stack-loop (create-label "L_fix_stack_loop"))
(define label-fix-stack-exit (create-label "L_fix_stack_exit"))
(define label-copy-frame-loop (create-label "L_copy_frame_loop"))
(define label-copy-frame-exit (create-label "L_copy_frame_exit"))
(define label-write-sob-exit (create-label "L_write_sob_exit"))

(define cg-if3
  	(lambda (pe ct fvars depth)
    	(let ( 	(if3-label (label-if3-else))
    			(if3-exit-label (label-if3-exit)))
      		(string-append
       			";;;;START OF IF \n\n"
    			(code-gen (car pe) ct fvars depth)
    			"cmp rax,SOB_FALSE\n"
    			"je " if3-label "\n"
    			(code-gen (cadr pe) ct fvars depth)
    			"jmp " if3-exit-label "\n"
    			if3-label ":\n"
    			(code-gen (caddr pe) ct fvars depth)
    			if3-exit-label ":"
    			"\n;;;;END OF IF\n\n"

    		))))

(define cg-or
  	(lambda (pe ct fvars depth)
    	(let ((or-exit-label (label-or-exit)))
      		(string-append
        		";;;;START OF OR \n\n"
    			(list-of-exps->str 
    				(map 	(lambda (x)
        						(string-append
    								(code-gen x ct fvars depth)
    								"cmp rax,SOB_FALSE \n"
    								"jne " or-exit-label "\n"))
    					pe))
    		or-exit-label ":"
     		"\n;;;;END OF OR\n\n"
))))

(define cg-fvar
  	(lambda (pe ct fvars depth)
    	(string-append
      	"mov rax, qword [" 
      	(search-addr (cadr pe) fvars)
      	"]\n"
)))

(define cg-pvar
  	(lambda (pe ct fvars depth)
    	(string-append
      		"mov rcx," 
      		(number->string (caddr pe))
      		"\nadd rcx,4\n"
      		"mov rax, qword [rbp + rcx*8]\n" 
      	)))

(define cg-def
  	(lambda (pe ct fvars depth)
    	(string-append
      		";;;;START OF DEFINE \n\n"
      		(code-gen (caddr pe) ct fvars depth)
      		";;;;END OF DEFINE \n\n"
      		"mov qword [" (search-addr (cadr (cadr pe)) fvars) "] ,rax\n"
      		"mov rax, SOB_VOID \n"
)))

(define cg-lambda-simple
  (lambda (pe ct fvars depth)
    (let ( 	(label-body (label-closure-body))
    		(label-exit (label-closure-exit))
    		(label-copy-params-loop (label-copy-params-loop))
    		(label-copy-params-exit (label-copy-params-exit))
    		(label-copy-prev-env-loop (label-copy-prev-env-loop))
    		(label-copy-prev-env-exit (label-copy-prev-env-exit)))
    	(string-append
      		"
      		mov rdi,8*" (number->string (+ 1 depth)) "
	      	call my_malloc
	      	mov rbx,rax
	      	mov rdi,qword [rbp+4*8]
	      	sal rdi,3
	      	call my_malloc
	      	mov rcx,rax
	      	mov rdi,8
	      	call my_malloc
	      	mov qword [rbx],rcx
	      	"
      		(if (> depth 0)
      			(string-append 
      				"
      				mov r8,0        
			      	" label-copy-params-loop ":
			      	cmp r8,qword [rbp+4*8]
			      	jge " label-copy-params-exit "
			      	mov rdx,qword [rbp+5*8+8*r8]
			      	mov qword [rcx + 4*r8],rdx
			      	inc r8
			      	jmp " label-copy-params-loop "\n"

      				label-copy-params-exit ":\n
			      	mov r8,0
			      	mov r9,1
			     	mov rdx,qword [rbp +3*8]
			     	"
			      	label-copy-prev-env-loop ":
			      	cmp r8, " (number->string depth) "
			      	jge " label-copy-prev-env-exit
			      	"
			      	mov rcx,qword [rdx + 8*r8]
			      	mov qword [rbx +8*r9],rcx
			      	inc r8
			      	inc r9
			      	jmp "  label-copy-prev-env-loop "\n"
			      	label-copy-prev-env-exit ":\n\n"
				) 
				"" ; this is the else( if depth = 0)
			) 
     	 	"
	      	MAKE_LITERAL_CLOSURE rax,rbx," label-body
	      	"\nmov rax,qword [rax]
	      	\njmp " label-exit "\n"

      		label-body ":
      		push rbp
      		mov rbp, rsp

        	"  (code-gen (getLambdaBody pe) ct fvars depth)
      		"leave
      		ret\n"
      		label-exit ":\n"
 ))))


(define cg-applic
  	(lambda (pe ct fvars depth)
    	(string-append
      		"\n;;;;;;;; START - APPLIC\n"
     		(apply string-append
   			"push SOB_NIL\n"
    		(map
      		(lambda (x)
        		(string-append
    				(code-gen x ct fvars depth)
    					"push rax\n"))
      					(reverse (caddr pe))))
      		"mov rcx, "(number->string (+ 1 (length (caddr pe))))
      		"\npush rcx\n"
      		(code-gen (cadr pe) ct fvars depth)
      		"
	      	mov rcx,rax
	      	TYPE rcx
	      	cmp rcx,T_CLOSURE
	      	jne L_cannot_apply_non_closure
	      	mov rbx,rax
	      	CLOSURE_ENV rbx
	      	push rbx
	      	CLOSURE_CODE rax
	     	call rax 
	      	add rsp,8*"
       		(number->string (+ 2 (+ 1 (length (caddr pe)))))
      
      		"\n ;;;;;;;; END - APPLIC   \n"
)))

;gets the free-vars table. constant table, parsed expressions(?), and depth (we need it for the lambdas).
;check pe. if it pe-seq, print it as sequence, if its pe-or, print it as OR, and so fourth.
;above, we can see the cg-SOMETHING, this prints the specific assembly.
 (define code-gen
  (lambda (pe ct fvars depth-count)
    (cond
    ((pe-seq? pe) 
    	(apply string-append (map (lambda (x) (code-gen x ct fvars depth-count)) (cadr pe))))
    ((pe-if3? pe) (cg-if3 (cdr pe) ct fvars depth-count))
    ((pe-or? pe) (cg-or (cadr pe) ct fvars depth-count))
    ((pe-lambda-simple? pe) (cg-lambda-simple pe ct fvars (+ 1 depth-count))) 
    ((pe-applic? pe) (cg-applic pe ct fvars depth-count))
    ((pe-pvar? pe) (cg-pvar pe ct fvars depth-count))
    ((pe-fvar? pe) (cg-fvar pe ct fvars depth-count))
    ((pe-def? pe) (cg-def pe ct fvars depth-count))
    ((pe-const? pe) 
    	(string-append
    		"mov rax, qword [" (search-addr (cadr pe) ct) "]\n" ))
    (else (error 'code-gen "ERROR code-gen else\n")))))

;after we evaluate an expression, the value of the expression located in RAX.
;this function prints the last value of the program.
; write_sob_if_not_void takes some unique type and print it according to the type.
; for example: if the type is PAIR - it prints it as PAIR
(define print-value
  	(lambda ()
    	(let 	((label-exit (label-write-sob-exit)))
      		(string-append 
  			"push rax\n";
  			"call write_sob_if_not_void \n"
  			"add rsp, 1*8 \n"))))


;---------------------------- compile-scheme-file -----------------------------------------
;The procedure compile-scheme-file takes the name of a Scheme source file (e.g., foo.scm),
;and the name of a x86 assembly target file (e.g., foo.s). It then performs the following:

; 1. 	Reads the contents of the Scheme source file, using the procedure file->string.
; 2. 	Reads the expressions in the string, using the reader you wrote in asssignment 1, returning a list of sexprs.
; 3. 	Applies to each sexpr in the above list the following, in order:
; 		– parse
; 		– remove-applic-lambda-nil
; 		– box-set
; 		– pe->lex-pe
;	 	– annotate-tc
;		This process is demonstrated in the pipeline function below.
; 4. 	Constructs the constants table, the symbol table (linked-list), and the global variable table.
; 5. 	Calls code-gen to generate a string of x86 assembly instructions for all the expressions. After
;		the code for evaluating each expression, there should appear a call to an assembly language
;		routine for printing to the screen the value of the expression (the contents of RAX) if it is not
;		the void object.


(define compile-scheme-file
  (lambda (scheme-file target-file)
    (let* ( 	(scheme-file-content (list->sexprs (string->list ;(string-append (scheme-primitive-functions)
                  (file->string scheme-file))))
     ;step 3
     			(parsed-exps  
        			(map (lambda (x)
    						(annotate-tc 
      							(pe->lex-pe
        							(box-set
          								(remove-applic-lambda-nil
        									(parse x))))))
        			scheme-file-content))
     ;step 4
        		(fvar-table (create-fvar-table parsed-exps next-free-ind-in-mem)) 
       			(constants-table (create-constants-table parsed-exps))
       			(out (open-output-file target-file))) ;display to the screen

    (display parsed-exps) ;just for checks
    (begin 
    	(display	
    		(string-append 

				" %include \"mayer/scheme.s\" \n\n "
				(constants-table->assembly constants-table)
				(fvar-table->assembly fvar-table)
				"
				  L_error_lambda_args_count:
				    call exit
				  L_cannot_apply_non_closure:
				    call exit
				  L_error_invalid_arguments:
				    call exit
				"

				"section .text
				main:\n\n"
				;       mov rbx,LZeroPredBody 
				;       mov rax,LGLOB1
				;       mov rcx,LGLOB2
				;       MAKE_LITERAL_CLOSURE rax, rcx, LZeroPredBody

				;        CLOSURE_CODE rax
				;        mov r9,111111111 
				;        call rax
				;        jmp lexit

				;       LZeroPredBody:
				        ; push rbp
				        ; mov rbp, rsp
				        ; mov rax,MAKE_LITERAL(T_INTEGER,3)
				        ; push rax
				        ; call write_sob_if_not_void ;;;;;;;;;;;;;;;; malloc 
				        ; add rsp, 1*8 \n

				        ; leave
				        ; ret

				  ;     lexit:
				  ; "

 				(primitive-zero-pred fvar-table)
				; mov rdi,8
				; call my_malloc
				; mov qword [rax],3
				; mov rbx,qword [rax]
				; MAKE_INT rbx
				; mov rax,rbx
				; push rax
				; call write_sob_if_not_void ;;;;;;;;;;;;;;;; malloc 
				; add rsp, 1*8 \n
				; ret \n
				; "
				 
				; mov qword [rax],3
				; mov rbx,qword [rax]
				; mov rax,MAKE_INT 1 
				; push rax
				; call write_sob_if_not_void 
				; add rsp, 1*8 
				; ret \n"
				; mov qword [temp],rax
				; mov rax,MAKE_LITERAL(T_INTEGER,rbx)

;step 5
				(list-of-exps->str 
				   (map (lambda (x) (string-append 
				    (code-gen x constants-table fvar-table -1) (print-value))) parsed-exps))

				"\n\nret") ; end of append

     		out) ; now print the code to the screen!
    	(close-output-port out))))) ; when display ends, close output-port.
    


















; (define compile-scheme-file
;   (lambda (scheme-file target-file)
;     (let* ((scheme-file-content (list->sexprs (string->list (string-append (scheme-primitive-functions)
; 							    (file->string scheme-file)))))
; 	   (parsed-exps  
; 	      (map (lambda (x)
; 		(annotate-tc 
; 		  (pe->lex-pe
; 		    (box-set
; 		      (remove-applic-lambda-nil
; 			(eliminate-nested-defines
; 			  (parse x)))))))
; 			  scheme-file-content))
; 	    (constants-table (create-constants-table parsed-exps))
; 	    (fvar-table (create-fvar-table parsed-exps next-free-ind-in-mem))
; 	    (constant-symbols (constant-symbols constants-table constants-table))
; 	    (symbol-table-addr next-free-ind-in-mem)
; 	    (out (open-output-file target-file)))
; 		(begin
; 		       (display
; 		       (string-append "
; #include <stdio.h>\n
; #include <stdlib.h>\n
; #define DO_SHOW 0
; #include \"arch/cisc.h\"
; #define SOB_FALSE 5
; #define SOB_TRUE 3
; #define SOB_NIL 2
; #define SOB_VOID 1
; int main()
; {
;   START_MACHINE;
  
;   JUMP(CONTINUE);
  
;   #include \"arch/char.lib\"
;   #include \"arch/io.lib\"  
;   #include \"arch/system.lib\"
;   #include \"arch/scheme.lib\"
;   #include \"arch/string.lib\"
;   #include \"arch/math.lib\"  
  
;   L_error_lambda_args_count:
;     SHOW(\"ERROR - invalid arguments number\\n\", R0);
;     HALT;
    
;   L_cannot_apply_non_closure:
;     SHOW(\"ERROR - cannot apply non closure\\n\", R0);
;     HALT;
    
;   L_error_invalid_arguments:
;     SHOW(\"ERROR - one or more of the arguments are invalid\\n\", R0);
;     HALT;    
  
;   CONTINUE:

;   // START - CONSTANTS TABLE CREATION \n"
;   (constants-table->assembly constants-table)
;   "// END - CONSTANTS TABLE CREATION \n"
;   "// MALLOC FOR FVAR TABLE\n"
;   (if (= (length fvar-table) 0) ""
;     (string-append "PUSH(IMM(" (number->string (length fvar-table)) "));
;   CALL(MALLOC);
;   DROP(1)\n" ))
;   "PUSH(IMM(1));
;    CALL(MALLOC);
;    DROP(1);
;    MOV(R1,R0);\n"
;   (constant-symbols->assembly constant-symbols)
;   (primitive-car fvar-table)
;   (primitive-cdr fvar-table)
;   (primitive-apply fvar-table)
;   (primitive-cons fvar-table)
;   (primitive-null-pred fvar-table)
;   (primitive-boolean-pred fvar-table)
;   (primitive-char-pred fvar-table)
;   (primitive-integer-pred fvar-table)
;   (primitive-pair-pred fvar-table)
;   (primitive-number-pred fvar-table)
;   (primitive-procedure-pred fvar-table)
;   (primitive-char-to-integer fvar-table)
;   (primitive-integer-to-char fvar-table)
;   (primitive-string-length fvar-table)
;   (primitive-denominator fvar-table)
;   (primitive-numerator fvar-table)
;   (primitive-make-string fvar-table)
;   (primitive-make-vector fvar-table)
;   (primitive-vector-length fvar-table)
;   (primitive-string-pred fvar-table)
;   (primitive-symbol-pred fvar-table)
;   (primitive-vector-pred fvar-table)
;    (primitive-zero-pred fvar-table)
;   (primitive-list-to-vector fvar-table)
;   (primitive-string-ref fvar-table)
;   (primitive-vector-ref fvar-table)
  
;   (primitive-reduce-num fvar-table)
;   (primitive-binary-int-frac-plus fvar-table)
;   (primitive-binary-int-int-plus fvar-table)
;   (primitive-binary-frac-frac-plus fvar-table)
;   (primitive-opposite-num fvar-table)
;   (primitive-inverse-num fvar-table)
;   (primitive-binary-int-int-mul fvar-table)
;   (primitive-binary-int-frac-mul fvar-table)
;   (primitive-binary-frac-frac-mul fvar-table)
;   (primitive-greater-than-int-int fvar-table)
;   (primitive-remainder fvar-table)
  
;   (primitive-string-set fvar-table)
;   (primitive-vector-set fvar-table)
;   (primitive-set-car fvar-table)
;   (primitive-set-cdr fvar-table)
  
;   (primitive-symbol-to-string fvar-table)
;   (primitive-string-to-symbol fvar-table symbol-table-addr)
  
;   (primitive-eq fvar-table)
  
;   (list-of-exps->str 
;     (map (lambda (x) (string-append (code-gen x constants-table fvar-table -1) (print-value))) parsed-exps))
;   "\n\n
  
;   STOP_MACHINE;
  
;   return 0;
; }")
				  
; 				  out)
; 				  (close-output-port out)))
; ))