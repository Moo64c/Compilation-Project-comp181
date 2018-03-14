
(define x (lambda (a b) (if (> (string-length a) b) (string-ref a b) a)))
(char->integer (x "hello" 3))