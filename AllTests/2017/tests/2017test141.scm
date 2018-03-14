
(map (lambda (x) (if (integer? x) (char->integer (integer->char x)) 0)) '(1 2 3 + #f))