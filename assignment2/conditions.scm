(define if3?
  (lambda (sxpr)
      (and
        (list? sxpr)
        (or
          (= 4 (length sxpr))
          (= 3 (length sxpr)) )
          (equal? 'if (car sxpr)
        )
      )
    )
  )


(define parse-if3
  (lambda (sxpr)
    (if (= 4 (length sxpr))
      `(if3 ,(parse (cadr sxpr)) ,(parse (caddr sxpr)) ,(parse (cadddr sxpr)))
      `(if3 ,(parse (cadr sxpr)) ,(parse (caddr sxpr)) ,`(const ,@(list (void))))
    )
  )
)

(define or?
  (lambda (sxpr)
    (and
      (list? sxpr)
      (< 0 (length sxpr))
      (equal? 'or (car sxpr))
    )
  )
)

(define parse-or
  (lambda (sxpr)
  (cond
    ((= 1 (length sxpr)) (parse #f))
    ((= 2 (length sxpr)) (parse (cadr sxpr)))
    (else `(or ,(map parse (cdr sxpr)))))
  )
)

(define and?
  (lambda (sxpr)
    (and
      (list? sxpr)
      (< 0 (length sxpr))
      (equal? 'and (car sxpr)))
  )
)

(define parse-and
  (letrec
    ((rex
      (lambda (_list)
        (if (= 1 (length _list))
          (parse (car _list))
          `(if3 ,(parse (car _list)) ,(rex (cdr _list)) ,(parse #f)))
      )
     )
    )
    (lambda (sxpr)
      (if (= 1 (length sxpr))
        (parse #t)
        (rex (cdr sxpr))
      )
    )
  )
)

(define cond?
  (letrec
    ((rex
      (lambda (_list)
        (cond
          (
            (and
              (= 1 (length _list))
              (pair? (car _list)))
          #t)
          (
            (and
              (pair? (car _list))
              (not (equal? 'else (caar _list)))
              (rex (cdr _list))
            )
          )
          (else #f)
         )
        )
      )
    )
    (lambda (sxpr)
      (and
        (list? sxpr)
        (< 1 (length sxpr))
        (equal? 'cond (car sxpr))
        (< 0 (length (cdr sxpr))) ;;
        (rex (cdr sxpr))
      )
    )
  )
)

(define parse-cond
	(letrec
    ((rex
      (lambda (cond_list)
  		  (cond
          ((and
            (= 1 (length cond_list))
            (equal? 'else (caar cond_list) ))
            (decompose-list (cdar cond_list)))
  			  (( = 1 (length cond_list)) `(if ,(caar cond_list) ,(decompose-list (cdar cond_list) )))
  			  (else `(if ,(caar cond_list) ,(decompose-list (cdar cond_list)) ,(rex (cdr cond_list))))
        )
      )
    )
   )
	 (lambda (sxpr)
    (parse (rex (cdr sxpr)))
    )
  )
)
