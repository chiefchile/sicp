(define (make-leaf symbol weight) (list 'leaf symbol weight)) 

(define (leaf? object) (eq? (car object) 'leaf)) 

(define (symbol-leaf x) (cadr x)) 

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right) 
  (list left 
	right 
	(append (symbols left) (symbols right)) 
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree)) 

(define (right-branch tree) (cadr tree)) 

(define (symbols tree) 
  (if (leaf? tree) 
      (list (symbol-leaf tree)) 
      (caddr tree))) 

(define (weight tree) 
  (if (leaf? tree) 
      (weight-leaf tree) 
      (cadddr tree)))

(define (decode bits tree) 
  (define (decode-1 bits current-branch) 
    (if (null? bits) '() 
	(let ((next-branch (choose-branch (car bits) current-branch))) 
	  (if (leaf? next-branch) 
	      (cons (symbol-leaf next-branch) 
		    (decode-1 (cdr bits) tree)) 
	      (decode-1 (cdr bits) next-branch))))) 
  (decode-1 bits tree))

(define (choose-branch bit branch) 
  (cond ((= bit 0) (left-branch branch)) 
	((= bit 1) (right-branch branch)) 
	(else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set) 
  (cond ((null? set) (list x)) 
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs) 
  (if (null? pairs) 
      '() 
      (let ((pair (car pairs))) 
	(adjoin-set (make-leaf (car pair) ; symbol 
			       (cadr pair)) ; frequency 
		    (make-leaf-set (cdr pairs))))))

(define sample-tree (make-code-tree (make-leaf 'A 4) 
				    (make-code-tree (make-leaf 'B 2) 
						    (make-code-tree (make-leaf 'D 1) 
								    (make-leaf 'C 1))))) 
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

(define (is-on-branch? symbol branch)
  (cond ((leaf? branch) (eq? (symbol-leaf branch) symbol))
	((not (leaf? branch)) (memq symbol (symbols branch)))
	(else false)))

(define (encode-symbol symbol tree)
  (define (iter tree result)
    (let ((left (left-branch tree))
	  (right (right-branch tree)))
      (cond ((and (is-on-branch? symbol left) (leaf? left))
	     (append result (list 0)))
	    ((and (is-on-branch? symbol left) (not (leaf? left)))
	     (iter left (append result (list 0))))
	    ((and (is-on-branch? symbol right) (leaf? right))
	     (append result (list 1)))
	    ((and (is-on-branch? symbol right) (not (leaf? right)))
	     (iter right (append result (list 1))))
	    (else (error "SYMBOL NOT IN TREE:" symbol)))))
  
  (iter tree nil))

(define (encode message tree) 
  (if (null? message) 
      '() 
      (append (encode-symbol (car message) tree) 
	      (encode (cdr message) tree))))

(decode (encode '(b c a d d c a b) sample-tree) sample-tree)

(define (successive-merge leaf-set)
  (define (iter tree)
    (let ((merged (make-code-tree (car tree) (cadr tree))))
      (if (null? (cddr tree))
	  merged
	  (iter (adjoin-set merged (cddr tree))))))
  (iter leaf-set))

(successive-merge (make-leaf-set '((D 3) (A 1) (B 1) (C 2))))

(define (generate-huffman-tree pairs) 
  (successive-merge (make-leaf-set pairs)))

(define generated-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

(decode (encode '(a d c b c a d) generated-tree) generated-tree)

(define rock-tree (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

(define song '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))

(length song)

(length (encode song rock-tree))

















