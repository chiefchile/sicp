(define (square x) (* x x))

(define (even? x) (= (remainder x 2) 0))

(define (odd? x) (not (even? x)))

(define nil '())

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n) 
	((divides? test-divisor n) test-divisor) 
	(else (find-divisor n (+ test-divisor 1))))) 

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n) (= n (smallest-divisor n)))

(define (enumerate-interval low high) 
  (if (> low high) 
      nil 
      (cons low (enumerate-interval (+ low 1) high))))

(define (print s)
  (display s)
  (newline))

(define (size lst)
  (fold-right (lambda (x y) (+ 1 y)) 0 lst))

(define (memq item x) 
  (cond ((null? x) false) 
	((eq? item (car x)) x) 
	(else (memq item (cdr x)))))

(define (element-of-set? x set) 
  (cond ((null? set) false) 
	((equal? x (car set)) true) 
	(else (element-of-set? x (cdr set)))))

(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum) 
  (if (pair? datum) 
      (car datum) 
      (error "Bad tagged datum: TYPE-TAG" datum))) 
(define (contents datum) 
  (if (pair? datum) 
      (cdr datum) 
      (error "Bad tagged datum: CONTENTS" datum)))




