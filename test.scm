(cons (list 1 2) (list 3 4))

(cons 1 2)

(list (list 1 2) (list 3 4))

(list 1 '())

(define nil '())

(append nil (list 1 nil 2))

(define (eli-deep-reverse lst) 
  (cond ((null? lst) nil) 
	((pair? (car lst)) 
	 (append 
	  (eli-deep-reverse (cdr lst)) 
	  (list (eli-deep-reverse (car lst))))) 
	(else 
	 (append 
	  (eli-deep-reverse (cdr lst))))))

(define (func lst)
  (caddr lst))

(func '(1 2 3))












  


 