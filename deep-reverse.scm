(define (deep-reverse lst)
  (cond ((null? lst) '())  
	((pair? (car lst)) (append (deep-reverse (cdr lst))
				   (list (deep-reverse (car lst)))))
	(else (append (deep-reverse (cdr lst))
		      (list (car lst))))))

(deep-reverse (list (list 1 2) 3 (list (list 4 5) 6)))

(define nil '())

(append nil (list 2 3))

(list nil 1 nil)
