
(define x (lambda (a b c) (if (> (string-length a) b) (string-set! a b c) a)))
(string->symbol (x "hello" 30 #\r))