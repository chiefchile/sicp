(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

(define (cube x) (* x x x))

(define (inc n) (+ n 1)) 

(define (sum-cubes a b) (sum cube a inc b))

(sum-cubes 1 10)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)

(define (factorial x) 
  (if (= x 0)
      1
      (product identity 1 inc x)))

(factorial 0)

(define (even? x) (= (remainder x 2) 0))

(define (pi-numerator a)
  (cond ((= a 0) 2)
	((even? a) (+ a 2))
	(else (+ a 3))))



(define (pi-denom a)
  (cond ((= a 0) 3)
	((even? a) (+ a 3))
	(else (+ a 2))))

(product pi-denom 0 inc 3)

(define (pi-approx n)
  (* 4
     (/ (product pi-numerator 0 inc n)
	(product pi-denom 0 inc n))))

(pi-approx 5)

(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product-recur term (next a) next b))))

(product-recur identity 1 inc 5)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum-a term a next b)
  (accumulate + 0 term a next b))

(sum-a identity 1 inc 4)

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)  
    (if (> a b)
	result
	(iter (next a) (combiner result
				 (term a)))))
  (iter a null-value))

(define (product-a term a next b)
  (accumulate-iter * 1 term a next b))

(product-a identity 1 inc 4)
  
(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
	((filter a) (combiner (term a)
			      (filtered-accumulate combiner null-value term (next a) next b filter)))
	(else (combiner null-value
			(filtered-accumulate combiner null-value term (next a) next b filter)))))


(define (expmod base exp m) (cond ((= exp 0) 1) ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m)) (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n) (define (try-it a) (= (expmod a n n) a)) (try-it (+ 1 (random (- n 1)))))

(DEFINE (FAST-PRIME? N TIMES) (COND ((= TIMES 0) TRUE) ((FERMAT-TEST N) (FAST-PRIME? N (- TIMES 1))) (ELSE FALSE)))

(DEFINE (PRIME? N) 
  (if (or (= n 1) 
	  (= n 0))
      false
      (fast-prime? n 5)))

(define (square x) (* x x))

(define (sum-squares-prime n)
  (filtered-accumulate + 0 square 1 inc n  prime?))

(sum-squares-prime 5)
  
(display "test")  
  
  
  
  
  
  


