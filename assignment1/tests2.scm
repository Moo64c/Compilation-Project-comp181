(load "sexpr-parser.scm")

;Tests for every ast
(define failed-tests (box 0))

(define assert-equal
    (lambda (test-name exp1 exp2)
    ;(display (string-append "Test " test-name ": "))
    ;(for-each display '("Test " (string<? test-name) ": "))
        (if (equal? exp1 exp2)
        (set-box! failed-tests (unbox failed-tests))
        ;(display "Passed\n")
        (begin
            (set-box! failed-tests (+ (unbox failed-tests) 1))
            (display (string-append "Test " test-name ": "))
            (display "\nFailed\n====>expected:") (display exp2) (display "\n====>got:     ") (display exp1) (display "\n"))
            )))




(assert-equal "Case 1"  (test-string <sexpr> "1") '((match 1) (remaining "")))
(assert-equal "Case 2"  (test-string <sexpr> "#t") '((match #t) (remaining "")))

(assert-equal "Case 3"  (test-string <sexpr> "(#\\f)") '((match (#\f)) (remaining "")) )

(assert-equal "Case 4" (test-string <sexpr> "()")  '((match ()) (remaining "")) )
(assert-equal "Case 5"  (test-string <sexpr> "\"a tab (\t) followed by a \\xa;\"") '((match "a tab (\t) followed by a \n") (remaining "")) )
;(assert-equal "Case 6"  (test-string <sexpr> "0notanumber0") '((match \x30;notanumber0) (remaining "")) )
;(assert-equal "Case 7"  (test-string <sexpr> "#(#\\lambda #\\space #\\x8)") ' )
(assert-equal "Case 8"  (test-string <sexpr> "'(#\\lambda (Param)
    ;add one
        ##Param + 1)") '((match '(#\λ (param) (+ param 1))) (remaining "")) )
(assert-equal "Case 9" (test-string <sexpr>
  "(let ((x '(1 2 -3/2)))(+ ## ##(car x) - ##(cadr x) - ##(caddr)))")
    '((match (let ([x '(1 2 -3/2)])(+ (- (- (car x) (cadr x)) (caddr)))))
  (remaining ""))
 )
(assert-equal "Case 10" (test-string <sexpr> "(apply eq? `(,(+ 1 2) ,@(list ##3)))")   '((match (apply eq? `(,(+ 1 2) ,@(list 3)))) (remaining "")) )

(assert-equal "Case 11"  (test-string <sexpr> "(andmap (lambda (x) (= x 3)) `(6/2 ,(/ 6 2)))")  '((match (andmap (lambda (x) (= x 3)) `(3 ,(/ 6 2))))
  (remaining "")) )
(assert-equal "Case 12"  (test-string <sexpr> "`(#;\"list of empty structures\"
    #() . (() \"\"))")   '((match `(#() () "")) (remaining "")) )
(assert-equal "Case 13"  (test-string <sexpr> "#%f(-3, ##(- 9/1 6/12))") '((match (f -3 (- 9 1/2))) (remaining "")) )
(assert-equal "Case 14"  (test-string <sexpr> "@##3-3")  '((match (cbname (- 3 3))) (remaining "")) )
(assert-equal "Case 15"   (test-string <sexpr> "(almost-fact {fact})") '((match (almost-fact (cbname fact))) (remaining "")) )
(assert-equal "Case 16"   (test-string <sexpr> "(define . (add1 .((lambda (x)
    (+ x -1/1) . ()))))") '((match (define add1 (lambda (x) (+ x -1)))) (remaining "")) )
(assert-equal "Case 17"   (test-string <sexpr> "(eq? @(add1 x) . (x . ()))") '((match (eq? (cbname (add1 x)) x)) (remaining "")) )
(assert-equal "Case 18"   (test-string <sexpr> "; cbname infix function call
#%1 + 2 * ##{##f(3)} ^4/5") '((match (+ 1 (* 2 (expt (cbname (f 3)) 4/5))))
  (remaining "")) )
(assert-equal "Case 19"  (test-string <sexpr> "(let ((x 1))
    (eq? (add1 x) @(add1 x)))")  '((match (let ([x 1]) (eq? (add1 x) (cbname (add1 x)))))
  (remaining ""))
 )
(assert-equal "Case 20"  (test-string <sexpr> "{(cbfunc #\\LAMBDA)}")  '((match (cbname (cbfunc #\λ))) (remaining "")) )



(assert-equal "Case 21"  (test-string <sexpr> "(define foo
    (lambda (n)
        ##(if(1, n*foo(n-1)))))") '(test-string <sexpr> "(define foo
    (lambda (n)
        ##(if(1, n*foo(n-1)))))") )


(assert-equal "Case 22"  (test-string <sexpr> "(define  x '`(,`1))")  '((match (define x '`(,`1))) (remaining "")) )
(assert-equal "Case 23"  (test-string <sexpr> "##1+2*3+4")  '((match (+ (+ 1 (* 2 3)) 4)) (remaining ""))
 )

(assert-equal "Case 24"  (test-string <sexpr> "#%a[0]+a[1]*a[2]/a[3]-a[4]**a[5]^(f(a[6]))") '((match (- (+ (vector-ref a 0)
              (/ (* (vector-ref a 1) (vector-ref a 2)) (vector-ref a 3)))
           (expt
             (vector-ref a 4)
             (expt (vector-ref a 5) (f (vector-ref a 6))))))
  (remaining "")) )


(assert-equal "Case 25" (test-string <sexpr> "#%a[##(fact @(add1 x)) +a[x-1]]") '((match (vector-ref
          a
          (+ (fact (cbname (add1 x))) (vector-ref a (- x 1)))))
  (remaining "")) )
(assert-equal "Case 26" (test-string <sexpr> "#;(this is a
    #;(nested sexpr comment)
    followed by more comment
    )#\\x263a")   '((match #\☺) (remaining "")) )


(assert-equal "Case 27" (test-string <sexpr> "#%-1+###;(not really here)2/4")  '((match (+ -1 1/2)) (remaining "")) )

(assert-equal "Case 28"  (test-string <sexpr> "#%1+2+3+4")  '((match (+ (+ (+ 1 2) 3) 4)) (remaining ""))
 )
(assert-equal "Case 29"  (test-string <sexpr> "(let ((result ##a[0] + 2 * a[1] + 3 ^ a[2/2] - a[3] * b[i][j][i + j]))
  result)")  '((match (let ([result (- (+ (+ (vector-ref a 0)
                               (* 2 (vector-ref a 1)))
                            (expt 3 (vector-ref a 1)))
                         (* (vector-ref a 3)
                            (vector-ref
                              (vector-ref (vector-ref b i) j)
                              (+ i j))))])
          result))
  (remaining "")) )
(assert-equal "Case 30"  (test-string <sexpr> "(let ((result (* n ##+3/4^3 + 2/7^5)))
  (* result (f #T) ##g(g(g(result, result), result), result)))")  '((match (let ([result (* n (+ (expt 3/4 3) (expt 2/7 5)))])
          (* result (f #t) (g (g (g result result) result) result))))
  (remaining "")) )
(assert-equal "Case 31"  (test-string <sexpr> "## a[a[a[a[a[0]]]]]")  '((match (vector-ref
          a
          (vector-ref
            a
            (vector-ref a (vector-ref a (vector-ref a 0))))))
  (remaining "")) )




(assert-equal "Case 32"  (test-string <sexpr> "(define pi ##-4/1 * atan(1))") '((match (define pi (* -4 (atan 1)))) (remaining "")) )
(assert-equal "Case 33"  (test-string <sexpr> "#%(2 + 3) * (4 + 5) ^ (7 - #%(cos (* x x)))") '((match (* (+ 2 3) (expt (+ 4 5) (- 7 (cos (* x x))))))
  (remaining "")) )


(assert-equal "Case 34"  (test-string <sexpr> "## 2 + #;3 8") '((match (+ 2 8)) (remaining "")) )
(assert-equal "Case 35"  (test-string <sexpr> "`(the answer is ##2 * 3 + 4 * 5 and this is the rest of the list)") '((match `(the answer is (+ (* 2 3) (* 4 5)) and this is the
              rest of the list))
  (remaining "")) )
(assert-equal "Case 36" (test-string <sexpr> "#% 4 +
5 +

6 *

7 ^ 3")  '((match (+ (+ 4 5) (* 6 (expt 7 3)))) (remaining "")) )
(assert-equal "Case 37"  (test-string <sexpr> "(+ 1
##
1 + 2 * 3 ^ 4
(third argument)
(fourth argument)
##fifth(argument)
)") '((match (+ 1 (+ 1 (* 2 (expt 3 4))) (third argument)
           (fourth argument) (fifth argument)))
  (remaining "")) )
(assert-equal "Case 38" (test-string <sexpr> "(#\\newline  #\\nul #\\page #\\return #\\space #\\tab #\\newLINE #\\nUL #\\paGE #\\retURN #\\spACE #\\TaB)")  '((match (#\newline #\nul #\page #\return #\space #\tab
          #\newline #\nul #\page #\return #\space #\tab))
  (remaining "")) )

(display "\n**FINISHED TESTS**\n")
(if (>  (unbox failed-tests) 0)
    (begin (display "failed ")
    (display (unbox failed-tests))
    (display " tests.\n"))
    (display "all tests passed :)\n"))
