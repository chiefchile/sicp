(define (length items) 
  (if (null? items) 
      0 
      (+ 1 (length (cdr items))))) 

(define (list-ref items n) 
  (if (= n 0) 
      (car items) 
      (list-ref (cdr items) (- n 1)))) 

(define (last-pair l)
  (list-ref l (- (length l) 1)))

(last-pair (list 23 72 149 34))

(define (append list1 list2) 
  (if (null? list1) 
      list2 
      (cons (car list1) (append (cdr list1) list2))))

(define nil '())

(define (reverse l)
  (if (< (length l) 2)
      l
      (append (reverse (cdr l))
	      (cons (car l) nil))))
	      

(reverse (list 1 4 9 16 25))		 
		 
