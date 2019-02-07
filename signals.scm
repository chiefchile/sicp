(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
	  (accumulate op initial (cdr sequence))))) 

(define (accumulate-n op init sequence) 
  (define nil '()) 
  (if (null? (car sequence)) 
      nil 
      (cons (accumulate op init (map car sequence)) 
	    (accumulate-n op init (map cdr sequence))))) 


(define (dot-product v w) 
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3) (list 4 5 6)) 

(define (matrix-*-vector m v) 
  (map (lambda (m-row) (dot-product m-row v))
       m))

(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))) 

(matrix-*-vector matrix (list 2 3 4 5)) 

(define nil '())

(define (transpose mat) 
  (accumulate-n cons   
		nil 
		mat))

(transpose matrix)

(define (matrix-*-matrix m n) 
  (let ((cols (transpose n))) 
    (map (lambda (m-row) (map (lambda (n-col) (dot-product m-row n-col)) 
			      cols))
	 m)))

(define m (list (list 1 2 3) (list 4 5 6)))
(define n (list (list 7 8) (list 9 10) (list 11 12)))

(matrix-*-matrix m n)

(fold-right / 1 (list 1 2 3)) 
(fold-left / 1 (list 1 2 3)) 
(fold-right list nil (list 1 2 3)) 
(fold-left list nil (list 1 2 3))

(define (reverse sequence) 
  (fold-right (lambda (x y) (append  y (list x))) 
	      nil 
	      sequence))

(reverse (list 1 2 3))

(define (reverse-fl sequence) 
  (fold-left (lambda (x y) (append (list y) x)) 
	     nil 
	     sequence))

(reverse-fl (list 1 2 3))
    
    
    
    
    
    
    
