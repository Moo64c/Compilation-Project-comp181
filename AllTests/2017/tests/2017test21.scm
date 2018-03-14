((lambda (f1 f2 input1 input2 input3 ifval)
      (if (ifval input2 input3)
      (f1 (f2 input1 5) 40)
      (begin
        (set! f2 f1)
        (f1 (f2 input1 5) 40)
        )
      )
 ) * + 5 7 -8 >)
