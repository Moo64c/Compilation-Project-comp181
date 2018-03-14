;;helper function to help remove 1 element
(define remove-from-list
  (lambda (lst var)
    (cond
      ((null? lst) lst)
      ((not (list? lst))
        (if (eq? lst var)
          '()
          lst
        )
      )
      ((eq? (car lst) var)
        (cdr lst)
      )
      (else
        `(,(car lst) ,@(remove-from-list (cdr lst) var))
      )
    )
  )
)


;;remove list2 from list1 = list1\list2
(define list-sub
  (lambda (list1 list2)
    (cond
      ((null? list1)
        '()
      )
      ((null? list2)
        list1
      )
      ((not (list? list2))
        (remove-from-list list1 list2)
      )
      ((not (list? list1))
        (list-sub (remove-from-list list1 (car list2)) (cdr list2))
      )
      (else
        (list-sub (remove-from-list list1 (car list2)) (cdr list2))
      )
    )
  )
)

;;checks if the var apears in a set
(define set-check
  (lambda (var)
    (lambda (body)
      (cond
        ((null? body) #f)
        ((not (list? body)) #f)
        ((eq? (car body) 'lambda-simple)
          ((set-check (list-sub  var (cadr body) )) (cddr body))
        )
        ((eq? (car body) 'lambda-opt)
          ((set-check (list-sub var (cons (cadr body) (caddr body) ))) (cdddr body))
        )
        ((eq? (car body) 'set)
          (if (eq? (car (cdadr body)) var)
            #t
            ((set-check var) (cddr body))
          )
        )
        (else
          (ormap (set-check var) body)
        )
      )
    )
  )
)

;;checks if the var is used
(define use-check
  (lambda (var)
    (lambda (body)
      (cond
        ((null? body) #f)
        ((not (list? body)) #f)
        ((eq? (car body) 'lambda-simple)
          ((use-check (list-sub  var (cadr body) )) (cddr body))
        )
        ((eq? (car body) 'lambda-opt)
          ((use-check (list-sub var (cons (cadr body) (caddr body) ))) (cdddr body))
        )
        ((eq? (car body) 'set)
          ((use-check var) (cddr body))
        )
        ((eq? (car body) 'var)
          (if (eq? (cadr body) var)
            #t
            #f
          )
        )
        (else (ormap (use-check var) body))
      )
    )
  )
)

(define bound-check
  (lambda (var)
    (lambda (body)
      (cond
        ((null? body) #f)
        ((not (list? body)) #f)
        ((eq? (car body) 'lambda-simple)
          (or
            ((set-check (list-sub  var (cadr body) )) (cddr body))
            ((use-check (list-sub  var (cadr body) )) (cddr body))
          )
        )
        ((eq? (car body) 'lambda-opt)
          (or
            ((set-check (list-sub var (append (cadr body) (caddr body) ))) (cdddr body))
            ((use-check (list-sub var (append (cadr body) (caddr body) ))) (cdddr body))
          )
        )
        (else
          (ormap (bound-check var) body)
        )
      )
    )
  )
)


;;predicat to check if a var is both in use and in set
(define to-box?
  (lambda (var body)
    (and
      ((use-check var) body)
      ((set-check var) body)
      ((bound-check var) body)
    )
  )
)


;;;find which vars meets the 3 criteria
(define to-box
  (lambda (vars body)
    (if (null? vars)
      '()
      (if
        (to-box? (car vars) body)
        (cons (car vars) (to-box (cdr vars) body))
        (to-box (cdr vars) body)
      )
    )
  )
)

;;checks if the var is in the list
;;(used as in if we get a var we check if it is in the vars that needed to be boxed)
(define var-in-list? (lambda (var list)
                       (cond ((null? list) #f)
                             ((eq? (car list) var) #t)
                             (else (var-in-list? var (cdr list))))))


;;;replacing all the box-vars inside the body of the lambda with box-get and set! to box-set
(define body-box (lambda (vars)
                   (lambda (body)
                     (cond ((null? body) body)
                           ((not (list? body)) body)
                           ((eq? (car body) 'lambda-simple) `(lambda-simple ,(cadr body) ,@((body-box (list-sub vars (cadr body))) (cddr body) )))
                           ((eq? (car body) 'lambda-opt) `(lambda-opt ,(cadr body) ,(caddr body)
                                                                      ,@((body-box (list-sub vars (append (cadr body) (list (caddr body))))) (cdddr body) )))
                           ((and (eq? (car body) 'set) (var-in-list? (car (cdadr body)) vars)) `(box-set ,(cadr body) ,@((body-box vars) (cddr body) )))
                           ((and (eq? (car body) 'var) (var-in-list? (cadr body) vars)) `(box-get ,body))
                           (else (map (body-box vars) body))))))


;;;boxing the vars that needed to be boxed in the beginning of the function
(define make-box (lambda (var)
                   `(set (var ,var) (box (var ,var)))))

;;helping functions to help remove unneccessary sequences
(define letrec-seq? (lambda (expr)
                      (cond ((not (list? (car expr))) #f)
                            ((eq? 'set (caar expr)) (letrec-seq? (cdr expr) ))
                            ((eq? 'seq (caar expr))  #t)
                            (else #f))))

(define remove-letrec-seq2 (lambda (expr)
                             (cond ((not (list? (car expr))) expr)
                                   ((eq? 'set (caar expr));
                                    (cons (car expr) (remove-letrec-seq2 (cdr expr))))
                                   ((eq? 'seq (caar expr)) (cadar expr))
                                   (else expr))
                             ))

(define remove-letrec-seq (lambda (expr)
                            (if (letrec-seq? (car expr))
                                (remove-letrec-seq2 (car expr))
                                expr)))

(define no-box-seq (lambda (x)
                     (if (and (eq? (caar x) 'seq) (letrec-seq? (cadar x)))
                         `((seq ,(remove-letrec-seq (cdar x))))
                         x)))


(define box-set
  (lambda (parsed-expr)
  	(cond
          ((null? parsed-expr) parsed-expr)
          ((not (list? parsed-expr)) parsed-expr)
          ((eq? (car parsed-expr) 'var) `(var ,(cadr parsed-expr)))
          ((eq? (car parsed-expr) 'lambda-simple)
           (let ((vars-to-box (to-box (cadr parsed-expr) (cddr parsed-expr))))
             (if (null? vars-to-box)
                 `(lambda-simple ,(cadr parsed-expr) ,@(no-box-seq (map box-set (cddr parsed-expr))))
                 (if (eq? 'seq (caaddr parsed-expr))
                     `(lambda-simple ,(cadr parsed-expr) (seq  (,@(map make-box vars-to-box) ,@(no-box-seq (box-set ((body-box vars-to-box) (car (cdaddr parsed-expr))))))))
                     `(lambda-simple ,(cadr parsed-expr) (seq  (,@(map make-box vars-to-box) ,@(no-box-seq (box-set ((body-box vars-to-box) (cddr parsed-expr)))))))
                     ))))
          ((eq? (car parsed-expr) 'lambda-opt)
           (let ((vars-to-box (append (to-box (cadr parsed-expr) (cdddr parsed-expr)) (to-box (list (caddr parsed-expr)) (cdddr parsed-expr)) )))
             (if (null? vars-to-box)
                 `(lambda-opt ,(cadr parsed-expr) ,(caddr parsed-expr) ,@(map box-set (cdddr parsed-expr)))
                 (if (eq? 'seq (car (cadddr parsed-expr)))
                     `(lambda-opt ,(cadr parsed-expr) ,(caddr parsed-expr) (seq (,@(map make-box vars-to-box) ,@(box-set ((body-box vars-to-box) (car (cdaddr (cdr parsed-expr))))))))
                     `(lambda-opt ,(cadr parsed-expr) ,(caddr parsed-expr) (seq (,@(map make-box vars-to-box) ,@(box-set ((body-box vars-to-box) (cdddr parsed-expr))))))
                     ))))
          (else  (map box-set  parsed-expr)))))
