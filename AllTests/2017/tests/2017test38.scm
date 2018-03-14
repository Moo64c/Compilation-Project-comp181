
(define (expmod a b m) 
  (cond ((= b 0) 1)
	((= (remainder b 2) 0) (remainder (expmod (remainder (* a a) m) (/ b 2) m) m))
	(else (remainder (* a (expmod a (- b 1) m)) m))))
   
(expmod 5 13 1)