;;; @file define CBName expressions for <sexpr> Parser.

(define <CBName>
  (new
    (*delayed (lambda ()
      <CBNameSyntax1>))
    (*delayed (lambda ()
      <CBNameSyntax2>))
    (*disj 2)
    (*pack (lambda (expression)
    ;display as (cbname expression)
      `(cbname ,expression)))
  done)
)

; "@<sexpr>" syntax.
; Should ignore ,@<sexpr> statements.
(define <CBNameSyntax1>
   (new
    (*parser (char #\@))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with (lambda (pref sexpr)
        sexpr
      ;Return only the expression.
      ))
  done)
)

; "{ <sexpr> }" syntax.
(define <CBNameSyntax2>
  (<ignored*> (new
    (*parser (char #\{))
    (*parser <skip>) *star
    (*caten 2)
    (*delayed (lambda ()
      <sexpr>))
    (*parser <skip>) *star
    (*parser (char #\}))
    (*caten 2)
    (*caten 3)
    (*pack-with (lambda (pref sexpr suffix)
      ;Return only the expression.
       sexpr)
    )
  done))
)
