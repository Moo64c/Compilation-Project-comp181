
(map (lambda (x) (if (string? x) (string->symbol x) 0)) '("a1" "b2" 3 + "cf"))