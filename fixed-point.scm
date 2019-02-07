(define tolerance 0.00001) 

(define (fixed-point f first-guess) 
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance)) 
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next) 
	  next (try next)))) 
  (try first-guess))

(let ((golden-ratio (fixed-point (lambda (x) (+ 1 
						(/ 1 x)))
				 1.0)))
  (display golden-ratio))

(let ((x-to-x (fixed-point (lambda (x) (/ (log 1000)
					  (log x)))
			   1.5)))
  (display x-to-x))

(define (average x y) (/ (+ x y)
			 2))
(let ((x-to-x-ad (fixed-point (lambda (x) (average x  (/ (log 1000)
							 (log x))))
			      1.5)))
  (display x-to-x-ad))

(define (cont-frac n d i k)
  (if (> i k)
      0    
      (/ (n i)
	 (+ (d i)
	    (cont-frac n d (+ i 1) k)))))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1 10)

(define (d-e i)
  (cond ((= i 1) 2)
	((= i 4) 4)
	((= i 7) 6)
	((= i 10) 8)
	(else 1)))

(+ (cont-frac (lambda (i) 1.0)  
	      (lambda (i) 
		(if (= (remainder i 3) 2) 
		    (/ (+ i 1) 1.5) 
		    1))  
	      1 
	      10)
   2)

(define (cont-frac-iter n d k) 
  (define (iter i result) 
    (if (= i 0) 
	result 
	(iter (- i 1)  
	      (/ (n i) (+ (d i) result))))) 
  (iter k 0.0)) 

(+ (cont-frac-iter (lambda (i) 1.0)  
		   (lambda (i) 
		     (if (= (remainder i 3) 2) 
			 (/ (+ i 1) 1.5) 
			 1))  
		   10)
   2)







