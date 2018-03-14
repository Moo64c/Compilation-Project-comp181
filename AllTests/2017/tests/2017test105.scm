
((lambda (a) (if (char? a) (char->integer a) (if (integer? a) (integer->char a) a))) #\x50)