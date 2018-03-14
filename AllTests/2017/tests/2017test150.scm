
((lambda (sym int)
   (if (symbol? sym) (begin
		       (set! a (symbol->string sym))
		       (string-set! a 2 (integer->char int))
		       a))) 'abc 33)