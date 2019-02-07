(define (flatmap proc seq) 
  (fold-right append nil (map proc seq)))

(define (prime-sum? pair) 
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair) (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n) 
  (map make-pair-sum (filter prime-sum? 
			     (flatmap (lambda (i) (map (lambda (j) (list i j)) 
						       (enumerate-interval 1 (- i 1)))) 
				      (enumerate-interval 1 n)))))

(prime-sum-pairs 6)

(define (permutations s) 
  (if (null? s) ; empty set? 
      (list nil) ; sequence containing empty set 
      (flatmap (lambda (x) (map (lambda (p) (cons x p)) 
				(permutations (remove x s)))) 
	       s)))

(define (remove item sequence) (filter (lambda (x) (not (= x item))) sequence))

(permutations (list 1 2 3))

(define (ordered-triples-sum n s) 
  (filter (lambda (lst) (= (fold-right + 0 lst) s)) 
	  (flatmap 
	   (lambda (i) 
	     (flatmap (lambda (j) 
			(map (lambda (k) (list i j k)) 
			     (enumerate-interval 1 (- j 1)))) 
		      (enumerate-interval 1 (- i 1)))) 
	   (enumerate-interval 1 n)))) 

(ordered-triples-sum 10 15)


(define (unique-pairs n) 
  (flatmap (lambda (i)  
	     (map (lambda (j) (list i j)) 
		  (enumerate-interval 1 (- i 1)))) 
	   (enumerate-interval 1 n))) 

(map (lambda (k)
       (map (lambda (i) 
	      (map (lambda (j) (list k i j)) 
		   (enumerate-interval 1 (- i 1))))
	    (enumerate-interval 1 (- k 1))))
     (enumerate-interval 1 4))

(unique-pairs 4)

(define (safe? board)
  (let ((last-queen (car board)))
      (define (iter board up-diagonal down-diagonal)
	(cond ((null? (cdr board)) true) 
	      ((= last-queen (cadr board)) false)
	      ((= up-diagonal (cadr board)) false)
	      ((= down-diagonal (cadr board)) false)
	      (else (iter (cdr board) (- up-diagonal 1) (+ down-diagonal 1)))))
      (iter board (- last-queen 1) (+ last-queen 1))))

(safe? (list 1 3 1))

(define empty-board '())

(define (adjoin-position new-row rest-of-queens)
  (cons new-row rest-of-queens))

(define (queens board-size) 
  (define (queen-cols k) 
    (if (= k 0) 
	(list empty-board)
	(filter (lambda (positions) (safe? positions)) 
		(flatmap (lambda (rest-of-queens) 
			   (map (lambda (new-row) (adjoin-position new-row rest-of-queens)) 
				(enumerate-interval 1 board-size))) 
			 (queen-cols (- k 1)))))) 
  (queen-cols board-size))

(queens 8)












