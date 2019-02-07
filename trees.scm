(define (entry tree) (car tree)) 
(define (left-branch tree) (cadr tree)) 
(define (right-branch tree) (caddr tree)) 
(define (make-tree entry left right) (list entry left right))

(define (tree->list-1 tree)
  (print tree)
  (if (null? tree)
      '() 
      (append (tree->list-1 (left-branch tree)) 
	      (cons (entry tree) 
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree) 
  (define (copy-to-list tree result-list) 
    (print result-list)
    (if (null? tree) 
	result-list 
	(copy-to-list (left-branch tree) 
		      (cons (entry tree) 
			    (copy-to-list (right-branch tree) result-list))))) 
  (copy-to-list tree '()))

(define one (make-tree 1 nil nil))
(define five (make-tree 5 nil nil))
(define eleven (make-tree 11 nil nil))
(define three (make-tree 3 one five))
(define nine (make-tree 9 nil eleven))
(define seven (make-tree 7 three nine))

(define one (make-tree 1 nil nil))
(define five (make-tree 5 nil nil))
(define eleven (make-tree 11 nil nil))
(define nine (make-tree 9 nil eleven))
(define seven (make-tree 7 five nine))
(define three (make-tree 3 one seven))

(define one (make-tree 1 nil nil))
(define seven (make-tree 7 nil nil))
(define eleven (make-tree 11 nil nil))
(define nine (make-tree 9 seven eleven))
(define three (make-tree 3 one nil))
(define five (make-tree 5 three nine))


(tree->list-2 five)

(define (list->tree elements) 
(car (partial-tree elements (length elements)))) 

(define (partial-tree elts n) 
(if (= n 0) 
    (cons '() elts) 
    (let ((left-size (quotient (- n 1) 2))) 
      (let ((left-result (partial-tree elts left-size))) 
	(let ((left-tree (car left-result)) 
	      (non-left-elts (cdr left-result)) 
	      (right-size (- n (+ left-size 1)))) 
	  (let ((this-entry (car non-left-elts)) 
		(right-result (partial-tree (cdr non-left-elts) right-size)))
	    (let ((right-tree (car right-result)) 
		  (remaining-elts (cdr right-result)))
	      (let ((result (cons (make-tree this-entry left-tree right-tree) remaining-elts)))
		(print result)
		result))))))))

(list->tree (list 1 2 3 4 5))

(define (make-record key value left right)
  (list key value left right))

(define (key record) (car record))

(define (value record) (cadr record))

(define (left-record record) (caddr record))

(define (right-record record) (cadddr record))

(define one (make-record 1 'one  nil nil))
(define seven (make-record 7 'seven nil nil))
(define eleven (make-record 11 'eleven nil nil))
(define nine (make-record 9 'nine seven eleven))
(define three (make-record 3 'three one nil))
(define five (make-record 5 'five three nine))

(define (lookup given-key set-of-records) 
(cond ((null? set-of-records) nil) 
      ((= given-key (key set-of-records)) (value set-of-records)) 
      ((< given-key (key set-of-records)) (lookup given-key (left-record set-of-records))) 
      ((> given-key (key set-of-records)) (lookup given-key (right-record set-of-records)))))

(lookup 7 five)











