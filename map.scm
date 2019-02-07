(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
	    (my-map f (cdr lst)))))

(my-map (lambda (x) (* x x))
	(list 1 2 3 4))

(define (tree-map f tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map f sub-tree)
	     (f sub-tree)))
       tree))

(tree-map (lambda (x) (* x x))
	  (list 1 2 (list 3 4 (list 5))))

(cons (list 1) (list 2 3)))

(car (cdr (cdr (cons '() (list 1 2)))))

(car (cdr (car (cdr (cdr (list 1 (cons 2 3) (list 4 5)))))))

(cdr (list 1))

(define (subsets s) 
  (if (null? s) 
      (list '())
      (let ((rest (subsets (cdr s)))) 
	(append rest 
		(map (lambda (x) (append (list (car s)) 
					 x))
		     rest)))))

(subsets (list 1 2 3))



















































































































































