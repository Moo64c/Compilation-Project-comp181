
(let ((str 'hello))
    (set! f1 (lambda () str))
    (set! f2 (lambda () (string->symbol str)))
	str
    )