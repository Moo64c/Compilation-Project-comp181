
(define parse
   (lambda (expr)
    (cond
      ((_const? expr)
         (parse-const expr))
      ((_var? expr)
         (parse-var expr))
      ((if3? expr)
         (parse-if3 expr))
      ((and? expr)
         (parse-and expr))
      ((or? expr)
         (parse-or expr))
      ((_lambda-simple? expr)
         (parse-lambda-simple expr))
      ((_lambda-opt? expr)
         (parse-lambda-opt expr))
      ((_lambda-var? expr)
         (parse-lambda-var expr))
      ((_define-reg? expr)
         (parse-define-reg expr))
      ((_define-mit? expr)
         (parse-define-mit expr))
      ((_assign? expr)
         (parse-assign expr))
      ((_let? expr)
        (parse-let expr))
      ((_let-star? expr)
        (parse-let-star expr))
      ((_letrec? expr)
        (parse-letrec expr))
      ((cond? expr)
         (parse-cond expr))
      ((begin? expr)
         (parse-begin expr))
      ((_applic? expr)
         (parse-applic expr))
      ((_seq? expr)
         (parse-seq expr))
      ((_quasiquote? expr)
         (parse-quasiquote expr))
  		(else "ERROR"))
   )
)
