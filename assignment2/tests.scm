(load "tag-parser.scm")
(display "========================STARTING TESTS===============")
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
            (display "Failed\n====>expected:") (display exp2) (display "\n====>got:") (display exp1) (display "\n"))
            )))


;Tests of __Constants__

(assert-equal "Constants F1.1"  (parse '(quote a)) '(const a))
(assert-equal "Constants F1.2"  (parse '(quote (a b c))) '(const (a b c)))
(assert-equal "Constants F1.3"   (parse '(quote (quote a b c))) '(const (quote a b c)))


;Tests of __Variables__

(assert-equal "Variables F2.1"  (parse 'abc) '(var abc))
(assert-equal "Variables F2.2"  (parse '123x) '(var 123x))

;Tests of __Conditionals__

(assert-equal "Conditionals F3.1"   (parse '(if a b c)) '(if3 (var a) (var b) (var c)))
(assert-equal "Conditionals F3.2"  (parse '(if (if a b c) 'x '(x y z))) '(if3 (if3 (var a) (var b) (var c)) (const x) (const (x y z))))
;(assert-equal "Conditionals F3.3"   (parse '(if a b)) '(if3 (var a) (var b) (const #<void>)))
;(assert-equal "Conditionals F3.4"  (parse '(if a 4)) '(if3 (var a) (const 4) (const #<void>)))
;(assert-equal "Conditionals F3.5"  (parse '(if #t 'abc)) '(if3 (const #t) (const abc) (const #<void>)))


;Tests of __Disjunctions__

(assert-equal "Disjunctions F4.1"   (parse '(or (zero? x) (zero? y) (zero? z))) '(or ((applic (var zero?) ((var x))) (applic (var zero?) ((var y))) (applic (var zero?) ((var z))))))
(assert-equal "Disjunctions F4.2"  (parse '(or (or (f1 x) (f2 y)) (or (f3 z) (f4 w) (f5 r)) (and (f6 u) (f7 t)))) '(or ((or ((applic (var f1) ((var x))) (applic (var f2) ((var y))))) (or ((applic (var f3) ((var z))) (applic (var f4) ((var w))) (applic (var f5) ((var r))))) (if3 (applic (var f6) ((var u))) (applic (var f7) ((var t))) (const #f)))))

;Tests of __Lambdas__

(assert-equal "Lambdas F5.1"  (parse '(lambda (x y z) (if x y z))) '(lambda-simple (x y z) (if3 (var x) (var y) (var z))))
(assert-equal "Lambdas F5.2"  (parse '(lambda () a)) '(lambda-simple () (var a)))
(assert-equal "Lambdas F5.3"  (parse '(lambda (x . rest) rest)) '(lambda-opt (x) rest (var rest)))
(assert-equal "Lambdas F5.4"  (parse '(lambda (x y z . rest) (if x y z))) '(lambda-opt (x y z) rest (if3 (var x) (var y) (var z))))
(assert-equal "Lambdas F5.5" (parse '(lambda args (if x y z))) '(lambda-opt () args (if3 (var x) (var y) (var z))))
(assert-equal "Lambdas F5.6"   (parse '(lambda args args)) '(lambda-opt () args (var args)))
(assert-equal "Lambdas F5.7"   (parse '(lambda (x y z) #t #f)) '(lambda-simple (x y z) (seq ((const #t) (const #f)))))
(assert-equal "Lambdas F5.8"   (parse '(lambda (x . rest) #t (if x y z))) '(lambda-opt (x) rest (seq ((const #t) (if3 (var x) (var y) (var z))))))
(assert-equal "Lambdas F5.9"   (parse '(lambda args #t #f)) '(lambda-opt () args (seq ((const #t) (const #f)))))
(assert-equal "Lambdas F5.10"   (parse '(lambda (x y z) #t)) '(lambda-simple (x y z) (const #t)))
(assert-equal "Lambdas F5.11"   (parse '(lambda (x . rest) #t)) '(lambda-opt (x) rest (const #t)))
(assert-equal "Lambdas F5.12"   (parse '(lambda args #t)) '(lambda-opt () args (const #t)))
(assert-equal "Lambdas F5.13"  (parse '(lambda (args) args)) '(lambda-simple (args) (var args)))
(assert-equal "Lambdas F5.14"  (parse '(lambda () 1 2 3)) '(lambda-simple () (seq ((const 1) (const 2) (const 3)))))
;Tests of __Define__

(assert-equal "Define F6.1"  (parse '(define x 5)) '(define (var x) (const 5)))
(assert-equal "Define F6.2"  (parse '(define x (lambda (x) x))) '(define (var x) (lambda-simple (x) (var x))))
(assert-equal "Define F6.3"   (parse '(define (id x) x)) '(define (var id) (lambda-simple (x) (var x))))
(assert-equal "Define F6.4"   (parse '(define (foo x y z) (if x y z))) '(define (var foo) (lambda-simple (x y z) (if3 (var x) (var y) (var z)))))
(assert-equal "Define F6.5"   (parse '(define (foo x y . z) (if x y z))) '(define (var foo) (lambda-opt (x y) z (if3 (var x) (var y) (var z)))))
(assert-equal "Define F6.6"  (parse '(define (list . args) args)) '(define (var list) (lambda-opt () args (var args))))


;Tests of __Assignments__

(assert-equal "Assignments F7.1"  (parse '(set! x 3)) '(set (var x) (const 3)))
(assert-equal "Assignments F7.2"   (parse '(set! v (f x))) '(set (var v) (applic (var f) ((var x)))))

;Tests of __Applications__

(assert-equal "Applications F8.1" (parse '(a)) '(applic (var a) ()))
(assert-equal "Applications F8.2" (parse '(a b c)) '(applic (var a) ((var b) (var c))))
(assert-equal "Applications F8.3" (parse '((a b) (a c))) '(applic (applic (var a) ((var b))) ((applic (var a) ((var c))))))
(assert-equal "Applications F8.4" (parse '(let ((x 4)) x)) '(applic (lambda-simple (x) (var x)) ((const 4))))

;Tests of __let__

(assert-equal "let F9.1" (parse '(let ((x '(3 4)) (y #t)) (if (y) x #f))) '(applic (lambda-simple (x y) (if3 (applic (var y) ()) (var x) (const #f))) ((const (3 4)) (const #t))))
(assert-equal "let F9.2" (parse '(let () #f)) '(applic (lambda-simple () (const #f)) ()))
(assert-equal "let F9.3" (parse '(let ((x 3)) #f #t)) '(applic (lambda-simple (x) (seq ((const #f) (const #t)))) ((const 3))))
(assert-equal "let F9.4" (parse '(let ([x 5]) (let ([x 2] [y x]) (list y x)))) '(applic (lambda-simple (x) (applic (lambda-simple (x y) (applic (var list) ((var y) (var x)))) ((const 2) (var x))))((const 5))))
(assert-equal "let F9.5" (parse '(let ((x (let ((y 2) (z 4)) (+ z y))) (y t)) t)) '(applic (lambda-simple (x y) (var t)) ((applic (lambda-simple (y z) (applic (var +) ((var z) (var y)))) ((const 2) (const 4))) (var t))))
;Tests of __letrec__

(assert-equal "letrec F10.1"  (parse '(letrec ([is-even? (lambda (n) (or (zero? n) (is-odd? (sub1 n))))] [is-odd? (lambda (n) (and (not (zero? n)) (is-even? (sub1 n))))]) (is-odd? 11))) '(applic (lambda-simple (is-even? is-odd?) (seq ((set (var is-even?) (lambda-simple (n) (or ((applic (var zero?) ((var n))) (applic (var is-odd?) ((applic (var sub1) ((var n))))))))) (set (var is-odd?) (lambda-simple (n) (if3 (applic (var not) ((applic (var zero?) ((var n))))) (applic (var is-even?) ((applic (var sub1) ((var n))))) (const #f)))) (applic (lambda-simple () (applic (var is-odd?) ((const 11)))) ())))) ((const #f) (const #f))))
(assert-equal "letrec F10.2" (parse '(letrec ((x 6)) x)) '(applic (lambda-simple (x) (seq ((set (var x) (const 6)) (applic (lambda-simple () (var x)) ())))) ((const #f))))
(assert-equal "letrec F10.3" (parse '(letrec ((x 6)) x y)) '(applic (lambda-simple (x) (seq ((set (var x) (const 6)) (applic (lambda-simple () (seq ((var x) (var y)))) ())))) ((const #f))))
(assert-equal "letrec F10.4" (parse '(letrec ((x 6) (y 7)) x y)) '(applic (lambda-simple (x y) (seq ((set (var x) (const 6)) (set (var y) (const 7)) (applic (lambda-simple () (seq ((var x) (var y)))) ())))) ((const #f) (const #f))))
(assert-equal "letrec F10.5" (parse '(letrec () x)) '(applic (lambda-simple () (applic (lambda-simple () (var x)) ())) ()))
(assert-equal "letrec F10.6" (parse '(letrec ((x (letrec ((y 2) (z 4)) (+ z y))) (y t)) t)) '(applic (lambda-simple (x y) (seq ((set (var x) (applic (lambda-simple (y z) (seq ((set (var y) (const 2)) (set (var z) (const 4)) (applic (lambda-simple () (applic (var +) ((var z) (var y)))) ())))) ((const #f) (const #f)))) (set (var y) (var t)) (applic (lambda-simple () (var t)) ())))) ((const #f) (const #f))))

;Tests of __let*__
(assert-equal "let* F11.1" (parse '(let* ((x y) (z s) (a g) (x y) (y z)) x)) '(applic (lambda-simple (x) (applic (lambda-simple (z) (applic (lambda-simple (a) (applic (lambda-simple (x) (applic (lambda-simple (y) (var x)) ((var z)))) ((var y)))) ((var g)))) ((var s)))) ((var y))))
(assert-equal "let* F11.2" (parse '(let* () x)) '(applic (lambda-simple () (var x)) ()))
(assert-equal "let* F11.3" (parse '(let* () x y z)) '(applic (lambda-simple () (seq ((var x) (var y) (var z)))) ()))
(assert-equal "let* F11.4" (parse '(let* ((x y)) (let* ((x y) (a t)) a))) '(applic (lambda-simple (x) (applic (lambda-simple (x) (applic (lambda-simple (a) (var a)) ((var t)))) ((var y)))) ((var y))))
(assert-equal "let* F11.5" (parse '(let* ((x (let* ((b a) (c d)) x y z)) (b t)) d)) '(applic (lambda-simple (x) (applic (lambda-simple (b) (var d)) ((var t)))) ((applic (lambda-simple (b) (applic (lambda-simple (c) (seq ((var x) (var y) (var z)))) ((var d)))) ((var a))))))


;Tests of __and*__
(assert-equal "and F12.1" (parse '(and a b c d e)) '(if3 (var a) (if3 (var b) (if3 (var c) (if3 (var d) (var e) (const #f)) (const #f)) (const #f)) (const #f)))
(assert-equal "and F12.2" (parse '(and)) '(const #t))
(assert-equal "and F12.3" (parse '(and a)) '(var a))
(assert-equal "and F12.4" (parse '(and a b)) '(if3 (var a) (var b) (const #f)))
(assert-equal "and F12.5" (parse '(and (and a b) (and c b) (or a b))) '(if3 (if3 (var a) (var b) (const #f)) (if3 (if3 (var c) (var b) (const #f)) (or ((var a) (var b))) (const #f)) (const #f)))


;Tests of __cond__
(assert-equal "cond F13.1"  (parse '(cond ((eq? a 5) (and 7)) (else (f 1)))) '(if3 (applic (var eq?) ((var a) (const 5))) (const 7) (applic (var f) ((const 1)))))
(assert-equal "cond F13.2"  (parse '(cond (x y) (y 5) (else 8))) '(if3 (var x) (var y) (if3 (var y) (const 5) (const 8))))
(assert-equal "cond F13.3"  (parse '(cond (x y z) (else 8 9))) '(if3 (var x) (seq ((var y) (var z))) (seq ((const 8) (const 9)))))
(assert-equal "cond F13.4"  (parse '(cond (x y (cond (#t 5) (else 'a))) (else 8 9))) '(if3 (var x) (seq ((var y) (if3 (const #t) (const 5) (const a)))) (seq ((const 8) (const 9)))))

;Tests of __qq__
(assert-equal "qq F14.1"  (parse '`(a 5 6)) '(const (a 5 6)))
(assert-equal "qq F14.2"  (parse '``(a 5 6)) '(const `(a 5 6)))
(assert-equal "qq F14.3"  (parse '`(a 5 ,@'(3 4))) '(const (a 5 3 4)))
(assert-equal "qq F14.4"  (parse '`(a 5 ,(a 5))) '(applic (var append) ((const (a 5)) (applic (var cons) ((applic (var a) ((const 5))) (const ()))))))
(assert-equal "qq F14.5"  (parse '`(a 5 ,@((lambda (x) (list x)) 6))) '(applic (var append) ((const (a 5)) (applic (lambda-simple (x) (applic (var list) ((var x)))) ((const 6))))))


;Tests of __begin__
(assert-equal "begin F15.1"  (parse '(begin (if x y (begin #t #f)) (begin 3 4 5) (begin (lambda (x) (begin #t #f))))) '(seq ((if3 (var x) (var y) (seq ((const #t) (const #f)))) (const 3) (const 4) (const 5) (lambda-simple (x) (seq ((const #t) (const #f)))))))
(assert-equal "begin F15.2"  (parse '(begin (begin a b c))) '(seq ((var a) (var b) (var c))))
(assert-equal "begin F15.3"  (parse '(begin 1 (begin 2) (lambda (x) (+ x x) (+ x 1)))) '(seq ((const 1) (const 2) (lambda-simple (x) (seq ((applic (var +) ((var x) (var x))) (applic (var +) ((var x) (const 1)))))))))
(assert-equal "begin F15.4" (parse '(begin y (begin x z))) '(seq ((var y) (var x) (var z))))
(assert-equal "begin F15.5" (parse '(lambda () (let ((x (begin y (begin x z)))) (begin '() '() (if x y (begin x y z)))))) '(lambda-simple () (applic (lambda-simple (x) (seq ((const ()) (const ()) (if3 (var x) (var y) (seq ((var x) (var y) (var z))))))) ((seq ((var y) (var x) (var z)))))))
(assert-equal "begin F15.6"  (parse '(begin (begin 1 2 (begin 3 4 (begin 5 6) 7)) 8)) '(seq ((const 1) (const 2) (const 3) (const 4) (const 5) (const 6) (const 7) (const 8))))


(display "Number of failed tests  ")
(display (unbox failed-tests))
