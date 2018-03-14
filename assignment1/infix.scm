; Assumes we've defined "number"

(define <powersymbol>
  (new
    (*parser (char #\^))
    (*parser (char #\*))
    (*parser (char #\*))
    (*caten 2)
    (*disj 2)
  done)
)

(define <infixprefixextensionprefix>
  (new
    (*parser (word "##"))
    (*parser (word "#%"))
    (*disj 2)
  done)
)

(define <infixexpression>
  (new
    (*delayed (lambda () <infixstep1>))
  done)
)

(define <InfixExtension>
  (<ignored*> (new
    (*parser <infixprefixextensionprefix>)
    (*parser <infixexpression>)
    (*caten 2)
    (*pack-with (lambda (prefix s) s))
  done))
)


(define <infixsymbol>
  (new
    ; All symbol chars are okay
    (*parser <SymbolChar>)
    ; Except for those -
    (*parser (char #\+))
    (*parser (char #\-))
    (*parser (char #\*))
    (*parser (char #\/))
    (*parser (char #\^))
    ; Double star (**)
    (*parser (char #\*))
    (*parser (char #\*))
    (*caten 2)
    (*disj 6)
    *diff
    *plus
    (*pack (lambda (s)
      ; Return symbol correctly
      (string->symbol (list->string s))
    ))
  done)
)

(define <infixsexprescape>
  (new
    (*delayed (lambda ()  <sexpr>))
  done)
)

; Infix order-of-operation steps:
; We step into each level, where the first to process is the lowest order
; and the last to process is the topmost.
; A terminal step will handle symbols.
; Handle add / subtract
(define <infixstep1>
  (new
    (*delayed (lambda ()
      <infixstep2> ))
    (*parser (skipped-char #\-))
    (*parser (skipped-char #\+))
    (*disj 2)
    (*delayed (lambda ()
      <infixstep2> ))
    (*caten 2)
    (*pack-with (lambda(part1 part2)  (cons part1 part2)))
    *star
    (*caten 2)
    (*pack-with (lambda (owner list)
      (fold-left (lambda (current next)
        `(,(string->symbol (string (car next))) ,current ,(cdr next))) owner list)))
  done)
)

; Handle Multiply / Divide
(define <infixstep2>
  (new
    (*delayed (lambda ()
      <infixstep3> ))
    (*parser (skipped-char #\*))
    (*parser (skipped-char #\/))
    (*disj 2)
    (*delayed (lambda ()
      <infixstep3> ))
    (*caten 2)
    (*pack-with (lambda(part1 part2) (cons part1 part2)))
    *star
    (*caten 2)
    (*pack-with (lambda (owner list)
      (fold-left (lambda (current next)
        `(,(string->symbol (string (car next))) ,current ,(cdr next))) owner list)))
  done)
)

; Handle exponents
(define <infixstep3>
  (new
    (*delayed (lambda () <infixstep4> ))
    (*parser (<ignored*> <powersymbol>))
    (*delayed (lambda () <infixstep4> ))
    (*caten 2)
    (*pack-with (lambda (powersym num2)
      num2))
    *star
    (*caten 2)
    (*pack-with (lambda (item list)
      (fold-right
        (lambda (current next)
          `(expt ,current  ,next))
        (car (reverse (cons item list)))
        (reverse(cdr (reverse (cons item list)))))))
  done)
)

; Handle "array get" expressions
(define <infixstep4>
  (new
    (*delayed (lambda () <infixstep5> ))
    (*parser (skipped-char #\[))
    ; array get expression could have a calculation nested inside
    (*delayed (lambda () <infixexpression> ))
    (*parser (skipped-char #\]))
    (*caten 3)
    (*pack-with (lambda (symb1 nested symb2) nested))
    *star
    (*caten 2)
    (*pack-with (lambda (owner child)
     (fold-left (lambda (expre next)
       `(vector-ref ,expre ,next)) owner child)))
  done)
)
; Handle function call expressions
(define <infixstep5>
  (<ignored*>  (new
    (*delayed (lambda ()
      <infixstep6>))
    (*parser (skipped-char #\())
    (*delayed (lambda ()
      <infixarglist>))
    (*parser <epsilon>)
    (*disj 2)
    (*parser (skipped-char #\)))
    (*caten 3)
    (*pack-with (lambda (pleft expre pright) expre))
    *star
    (*caten 2)
    (*pack-with (lambda (owner list)
     (fold-left (lambda (expre next)
       `(,expre ,@next)) owner list)))
    (*delayed (lambda ()
     <infixstep6>))
    (*disj 2)
  done))
)

; Negative number handler; function call9
(define <infixstep6>
  (<ignored*>  (new
    (*parser (char #\-))
    (*parser <Number>)
    (*caten 2)
    (*pack-with (lambda (sign num) (- num)))
    ; Negative expression - starts at multiply/divide
    (*parser (char #\-))
    (*parser <infixstep2>)
    (*caten 2)
    (*pack-with (lambda (sign num) (- num)))
    ; Negative expression - starts at add/subtract
    (*parser (skipped-char #\())
    (*delayed (lambda () <infixstep1>))
    (*parser (skipped-char #\)))
    (*caten 3)
    (*pack-with (lambda (pleft expre pright) expre))
    ; Last possible options
    (*delayed (lambda ()
      <infixtermina_listep>))
    (*disj 4)
  done)))

; Handle exponents
(define <infixtermina_listep>
  (<ignored*> (new
     (*parser <Number>)
     (*parser <infixsymbol>)
     (*parser <infixsexprescape>)
     (*disj 3)
     done))
)

(define <infixstep1wrapper>
  (<ignored*> (new
    (*parser <infixstep1>)
  done))
)

(define <infixarglist>
  (<ignored*> (new
    ; InfixExp (, InfixExp)* | \epsilon
    (*parser <infixstep1wrapper>)
    (*parser (skipped-char #\,))
    (*parser <infixstep1wrapper>)
    (*caten 2)
    (*pack-with (lambda (comma expre) expre))
    *star
    (*caten 2)
    (*pack-with (lambda (comma expre) `(,comma ,@expre)))
  done))
)
