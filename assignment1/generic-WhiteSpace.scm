(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space)))
)

(define <line-comment>
  (let (
		(<end-of-line-comment>
		  (new
				(*parser <end-of-input>)
				(*parser (char #\newline))
		    (*disj 2)
		  done)))
    (new
			(*parser (char #\;))
	  	(*parser <any-char>)
	 	  (*parser <end-of-line-comment>)
	    *diff
			*star
	 	  (*parser <end-of-line-comment>)
	 	  (*caten 3)
	 done))
)

(define <sexpr-comment>
  (new
    (*parser (word "#;"))
    (*delayed (lambda ()
			<sexpr>))
    (*caten 2)
    done)
)

(define <comment>
  (disj
		<sexpr-comment>
		<line-comment>)
)

(define <skip>
  (disj <comment>
	<whitespace>)
)

(define <ignored-parent>
  (lambda (<parent>)
    (lambda (<p>)
      (new (*parser <parent>)
	   	(*parser <p>)
	   	(*parser <parent>)
	   	(*caten 3)
	   	(*pack-with
	      (lambda (_left e _right) e))
	  done)))
)

(define <ignored*>
	(<ignored-parent> (star <skip>)))

(define skipped-char (lambda (ch)
  (<ignored*> (char ch)))
)
