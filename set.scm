(define (adjoin-set element set)
  (cond ((null? set) (list element))
	((< element (car set)) (cons element set))
	((= element (car set)) set)
	(else (cons (car set) (adjoin-set element (cdr set))))))

(adjoin-set 10 (list 2 4 7))

(define (element-of-set? x set) 
  (cond ((null? set) false) 
	((= x (car set)) true) 
	((< x (car set)) false) 
	(else (element-of-set? x (cdr set)))))



(intersection-set (list 1 2 3) (list 1 4 5))

(define (union-set set1 set2)
  (cond ((null? set1) set2) 
	((null? set2) set1)
	((= (car set1) (car set2)) (union-set (cdr set1) set2))
	((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
	(else (cons (car set2) (union-set set1 (cdr set2))))))

(union-set (list 1 2 3 7 10 32) (list 1 4 5 45))