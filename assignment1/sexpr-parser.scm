

(define <sexpr>
 (<ignored*> (new
    (*delayed (lambda () <Boolean>))
    (*delayed (lambda () <Number>))
    (*delayed (lambda () <Symbol>))
    (*parser (range #\0 #\9))
    *diff
    *not-followed-by
    (*delayed (lambda () <Char>))
    (*delayed (lambda () <String>))
    (*delayed (lambda () <Symbol>))
    (*delayed (lambda () <ProperList>))
    (*delayed (lambda () <ImproperList>))
    (*delayed (lambda () <Vector>))
    (*delayed (lambda () <Quoted>))
    (*delayed (lambda () <QuasiQuoted>))
    (*delayed (lambda () <Unquoted>))
    (*delayed (lambda () <UnquoteAndSpliced>))
    (*delayed (lambda () <CBName>))
    (*delayed (lambda () <InfixExtension>))
    (*disj 14)
  done))
)

;checked
(define <Boolean>
  (new
    (*parser (char #\#))
    (*parser (char #\f))
    (*caten 2)
    (*pack-with
      (lambda (x y) #f)
    )
    (*parser (char #\#))
    (*parser (char #\t))
    (*caten 2)
    (*pack-with
       (lambda (x y) #t)
    )
    (*disj 2)
  done)
)

;checked
(define <CharPrefix>
  (new
    (*parser (char #\#))
    (*parser (char #\\))
    (*caten 2)
  done)
)

;checked
(define <Char>

  (new
    (*parser <CharPrefix>)
    (*delayed (lambda () <HexUnicodeChar>))
    (*delayed (lambda () <VisibleSimpleChar>))
    (*delayed (lambda () <NamedChar>))
    (*pack
      (lambda (x)
        (list->string (list x))
      )
    )
    (*disj 3)
    (*pack (lambda (n)
      (if (integer? n)
          (integer->char n)
          n)))
    (*caten 2)
    (*pack-with (lambda (x y) y))
  done)

)
;check 1/2
(define <NamedChar>
   (new
    (*parser (word-ci "lambda"))
    (*pack (lambda (cp) (integer->char 955)))
    (*parser <any-char>)
    *not-followed-by
    (*parser (word-ci "newline"))
    (*pack (lambda (cp) (integer->char 10)))
    (*parser <any-char>)
    *not-followed-by
    (*parser (word-ci "nul"))
    (*pack (lambda (cp)(integer->char 0)))
    (*parser <any-char>)
    *not-followed-by
    (*parser (word-ci "page"))
    (*pack (lambda (cp)(integer->char 12)))
    (*parser <any-char>)
    *not-followed-by
    (*parser (word-ci "return"))
    (*pack (lambda (cp)(integer->char 13)))
    (*parser <any-char>)
    *not-followed-by
    (*parser (word-ci "space"))
    (*pack (lambda (cp)(integer->char 32)))
    (*parser <any-char>)
    *not-followed-by
    (*parser (word-ci "tab"))
    (*pack (lambda (cp)(integer->char 9)))
    (*parser <any-char>)
    *not-followed-by
    (*disj 7)
  done)
)

(define <String>
  (new
    (*parser (char #\"))
    (*delayed (lambda ()
      <StringChar>))
    *star
    (*parser (char #\"))
    (*caten 3)
    (*pack-with (lambda (x str y)
      (list->string str)))
  done)
)

(define <StringChar>
  (new
    (*delayed (lambda () <StringHexChar>))
    (*delayed (lambda () <StringLiteralChar>))
    (*delayed (lambda () <StringMetaChar>))
    (*disj 3)
  done)
)

(define <VisibleSimpleCharO>
  (new
    (*parser (range #\! #\~))
	  (*pack (lambda (ch) ch))
    (*parser <any-char>)
    *not-followed-by
	done)
)

(define <VisibleSimpleChar>
       (new (*parser <any-char>)
            (*parser (range (integer->char 0) (integer->char 32)))
            *diff
        done))

(define <HexDigit>
  (let (
    (zero (char->integer #\0))
    (lc-a (char->integer #\a))
	  (uc-a (char->integer #\A)))
    (new
      (*parser (range #\0 #\9))
      (*pack (lambda (ch)
  	    (- (char->integer ch) zero)))
      (*parser (range #\a #\f))
      (*pack (lambda (ch)
        (+ 10 (- (char->integer ch) lc-a))))
      (*parser (range #\A #\F))
      (*pack (lambda (ch)
        (+ 10 (- (char->integer ch) uc-a))))
      (*disj 3)
    done)
  )
)

(define <HexChar>
  (new
    (*parser (range #\0 #\9))
    (*parser (range #\a #\f))
    (*parser (range #\A #\F))
    (*disj 3)
  done)
)

(define <StringHexChar>
  (new
    (*parser (char #\\))
    (*parser (char #\x))
    (*parser <HexChar>) *star
    (*parser (char #\;))
    (*caten 4)
    (*pack-with (lambda (sl x hex p)
      (integer->char (string->number (list->string `(#\# ,x ,@hex))))))
  done)
)

(define <HexUnicodeCharO>
  (new
    (*parser (char-ci #\x))
    (*parser <HexDigit>) *plus
	  (*caten 2)
    (*pack-with (lambda (x y)
      (fold-left (lambda (a b)
        (+ b (* a 16))) 0 y)))
    (*parser (range #\g #\z))
    (*parser (range #\g #\z))
    (*disj 2)
    *not-followed-by
  done)
)

(define <HexUnicodeChar>
  (new
    (*parser <HexDigit>)
    (*parser (char #\x))
    *plus
    (*caten 2)
    (*pack-with
      (lambda (x y)
        (fold-left (lambda (a b)
          (+ b (* a 16))) 0 y)
      )
    )
  done)
)


(define <Natural>
  (new
    (*parser (range #\0 #\9)) *plus
    (*pack (lambda (x)
      (string->number (list->string  x))))
   done)
)

(define <Integer>
  (new
    (*parser (char #\+))
    (*parser <Natural>)
    (*caten 2)
    (*pack-with (lambda (S+ n)  n))
    (*parser (char #\-))
    (*parser <Natural>)
    (*caten 2)
    (*pack-with (lambda (S- n) (- n)))
    (*parser <Natural>)
    (*disj 3)
  done)
)

(define <Fraction>
  (new
    (*parser <Integer>)
    (*parser (char #\/))
    (*caten 2)
    (*pack-with
      (lambda (i x) i))
    (*parser (char #\0))
    *not-followed-by
    (*parser <Natural>)
    (*caten 2)
    (*pack-with
      (lambda (i n)
        (/ i n)))
  done)
)

(define <Number>
  (new
    (*parser <Fraction>)
    (*parser <Integer>)
    (*disj 2)
    (*parser (range #\a #\z))
    (*parser (range #\? #\Z))
    (*parser (char #\/))
    (*disj 3)
    *not-followed-by
  done)
)

(define <StringMetaChar>
  (new
    (*parser (char #\\))
    (*parser (char #\\))
    (*parser (char #\"))
    (*parser (char #\t))
    (*parser (char #\f))
    (*parser (char #\n))
    (*parser (char #\r))
    (*disj 6)
    (*caten 2)
    (*pack-with (lambda (x y) y))
  done)
)

(define <StringLiteralChar>
  (new
    (*parser <any-char>)
    (*parser (char #\\))
    (*parser (char #\"))
    (*disj 2) *diff
    (*pack (lambda (x) x))
  done)
)

(define <SymbolChar>
  (new
    (*parser (range #\0 #\9))
    (*parser (range #\a #\z))
    (*parser (range #\A #\Z))
    (*pack (lambda (n)
      (integer->char (+ (char->integer n) 32))))
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
  done)
)

(define <Symbol>
  (new
    (*parser <SymbolChar>) *plus
    (*pack (lambda (x)
      (string->symbol (list->string x))))
  done)
)

(define <ProperList>
  (new
    (*parser (char #\())
    (*delayed (lambda () <sexpr>)) *star
    (*parser (char #\)))
    (*caten 3)
    (*pack-with (lambda (x y z) y))
  done)
)

(define <ImproperList>
  (new
    (*parser (char #\())
    (*delayed (lambda () <sexpr>)) *plus
    (*parser (char #\.))
    (*delayed (lambda () <sexpr>))
    (*parser (char #\)))
    (*caten 5)
    (*pack-with (lambda (open _list dot el close)
      (append _list el)))
  done)
)

(define <Vector>
  (new
    (*parser (char #\#))
    (*parser (char #\())
    (*delayed (lambda () <sexpr>)) *star
    (*parser (char #\)))
    (*caten 4)
    (*pack-with (lambda (a open vec close)
      (list->vector vec)))
  done)
)

(define <Quoted>
  (new
    (*parser (char #\'))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with (lambda (x y) `',y))
  done)
)

(define <QuasiQuoted>
  (new
    (*parser (char #\`))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with (lambda (x y)
      (list 'quasiquote y)))
  done)
)

(define <Unquoted>
  (new
    (*parser (char #\,))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with (lambda (x y)
      (list 'unquote y)))
  done)
)

(define <UnquoteAndSpliced>
  (<ignored*> (new
    (*parser (char #\,))
    (*parser (char #\@))
    (*delayed (lambda () <sexpr>))
    (*caten 3)
    (*pack-with (lambda (x y z)
      (list 'unquote-splicing z)))
  done))
)
