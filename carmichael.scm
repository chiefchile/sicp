(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m)) 
	(else (remainder (* base (expmod base (- exp 1) m)) 
			 m))))

(define (fermat-test a n)  (= (expmod a n n) a)) 

(define (fast-prime? a n) 
  (cond ((= a n) true) 
	((fermat-test a n) (fast-prime? (+ a 1) n)) 
	(else false)))

(fast-prime? 2 11)
