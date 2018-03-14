;; Created by Amir Arbel and Ronen Finish

(define annotate-tc-wrapper (lambda (expr)
  (letrec
    (
      (annotate-tc-map
        (lambda (_list tp?)
          (cond
            ((null? _list)
              '()
            )
            ((= 1 (length _list))
              (list (anotate (car _list) tp?))
            )
            (else
              (cons
                (anotate (car _list) #f)
                (annotate-tc-map (cdr _list) tp?)
              )
            )
          )
         )
      )
      (anotate
        (lambda (expr tp?)
          (cond
            ((not (list? expr))
              expr
            )
            ((is-var-or-const? expr)
              expr
            )
            ((or? expr)
              `(or ,(annotate-tc-map (cadr expr) tp?))
            )
            ((is-seq? expr)
              `(seq ,(annotate-tc-map (cadr expr) tp?))
            )
            ((is-if3? expr)
             `(if3 ,(anotate (cadr expr) #f)  ,(anotate (caddr expr) tp?)  ,(anotate (cadddr expr) tp?))
            )
            ((is-def? expr)
              `(def ,(cadr expr) ,(anotate (caddr expr) tp?))
            )
            ((is-lambda? expr)
              `(,@(header expr) ,(anotate (get-lambda-body expr) #t))
            )
            ((and
               tp?
               (is-applic? expr)
              )
              `(tc-applic ,(anotate (cadr expr) #f) ,(annotate-tc-map(caddr expr) #f))
            )
            ((is-applic? expr)
              `(applic ,(anotate (cadr expr) #f) ,(annotate-tc-map(caddr expr) #f))
            )
            (else
              (map (lambda (e) (anotate e #f)) expr)
            )
          )
        )
      )
    )
    (anotate expr #f)
  )
 )
)
