(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y)) 
(define (mul x y) (apply-generic 'mul x y)) 
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package) 
(define (tag x) (attach-tag 'scheme-number x)) 
(put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y)))) 
(put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y)))) 
(put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y)))) 
(put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y)))) 
(put 'make 'scheme-number (lambda (x) (tag x))) 
(put 'equ? '(scheme-number scheme-number) =)
'done)

(install-scheme-number-package)

(define (make-scheme-number n) ((get 'make 'scheme-number) n))

(define (install-rational-package) 
;; internal procedures 
(define (numer x) (car x)) 
(define (denom x) (cdr x)) 
(define (make-rat n d) 
  (let ((g (gcd n d))) (cons (/ n g) (/ d g)))) 
(define (add-rat x y) 
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y)))) 
(define (sub-rat x y) 
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y)))) 
(define (mul-rat x y) 
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y)))) 
(define (div-rat x y) 
  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y)))) 
(define (equ? x y)
  (and (= (numer x) (numer y))
       (= (denom x) (denom y))))
;; interface to rest of the system 
(define (tag x) (attach-tag 'rational x))
(put 'add '(rational rational) (lambda (x y) (tag (add-rat x y)))) 
(put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y)))) 
(put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y)))) 
(put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
(put 'make 'rational (lambda (n d) (tag (make-rat n d)))) 
(put 'equ? '(rational rational) equ?)
'done)

(install-rational-package) 

(define (make-rational n d) ((get 'make 'rational) n d))

(define (install-complex-package) 
;; imported procedures from rectangular and polar packages 
(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y)) 
(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a)) 
;; internal procedures 
(define (add-complex z1 z2) 
  (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2)))) 
(define (sub-complex z1 z2) 
  (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2)))) 
(define (mul-complex z1 z2) 
  (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2)))) 
(define (div-complex z1 z2) 
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))
(define (equ? z1 z2)
  (and (= (real-part z1) (real-part z2))
       (= (imag-part z1) (imag-part z2))))
;; interface to rest of the system 
(define (tag z) (attach-tag 'complex z))
(put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2)))) 
(put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2)))) 
(put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2)))) 
(put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2)))) 
(put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y)))) 
(put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a)))) 
(put 'real-part '(complex) real-part) 
(put 'imag-part '(complex) imag-part) 
(put 'magnitude '(complex) magnitude) 
(put 'angle '(complex) angle)
(put 'equ? '(complex complex) equ?)
'done)

(install-complex-package)

(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y)) 
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))

(define (attach-tag type-tag contents) 
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum) 
  (cond ((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum) 
  (cond ((number? datum) datum) 
	((pair? datum) (cdr datum))
	(else (error "Bad tagged datum: CONTENTS" datum))))

(define complex (make-complex-from-real-imag 1 2))
(contents complex)
(define complex2 (make-complex-from-real-imag 3 4))
(add complex complex2)
(real-part complex)

(define n1 (make-scheme-number 1))
(define n2 (make-scheme-number 2))
(add n1 n2)
(sub n2 n1)
(mul n1 n2)
(div n1 n2)

(define (equ? x y) (apply-generic 'equ? x y))
(equ? n1 n2)
(define rat1 (make-rational 1 2))
(define rat2 (make-rational 3 4))
(equ? rat1 rat2)
(add rat1 rat2)
(equ? complex complex2)

(define *coercion-table* (make-hash-table))

(define (put-coercion type1 type2 proc)
  (hash-table/put! *coercion-table* (list type1 type2) proc))

(define (get-coercion type1 type2)
  (hash-table/get *coercion-table* (list type1 type2) #f))

(define (scheme-number->complex n) (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

#|(put-coercion 'complex 'scheme-number scheme-number->complex)|#

(define (apply-generic op . args) 
(let ((type-tags (map type-tag args))) 
  (let ((proc (get op type-tags))) 
    (if proc 
	(apply proc (map contents args)) 
	(if (= (length args) 2) 
	    (let ((type1 (car type-tags)) 
		  (type2 (cadr type-tags)) 
		  (a1 (car args)) 
		  (a2 (cadr args))) 
	      (let ((t1->t2 (get-coercion type1 type2)) 
		    (t2->t1 (get-coercion type2 type1))) 
		(cond (t1->t2 (apply-generic op (t1->t2 a1) a2)) 
		      (t2->t1 (apply-generic op a1 (t2->t1 a2))) 
		      (else (error "No method for these types" (list op type-tags)))))) 
	    (error "No method for these types" (list op type-tags)))))))

(apply-generic 'add n1 complex)

(define (coerce type arg)
  (let ((arg-type (type-tag arg))) 
    (let ((arg-type->type (get-coercion arg-type type)))
      (if arg-type->type
	  (arg-type->type arg)
	  false))))

(coerce 'complex n1)

(define (coerce-args type args)
  (define (iter args result)
    (if (null? args)
	result
	(let ((arg-type (type-tag (car args))))
	  (if (not (eq? type arg-type)) 
	      (let ((coerced (coerce type (car args))))
		(if coerced
		    (iter (cdr args) 
			  (append result 
				  (list coerced)))
		    false))
	      (iter (cdr args)
		    (append result (list (car args))))))))
  (iter args nil))

(coerce-args 'complex (list n1 n2)) 

(define (try-all-coercions args)
  (define (iter types)
    (if (null? types)
	false
	(let ((coerced (coerce-args (car types) args)))
	  (if coerced
	      coerced
	      (iter (cdr types))))))
  (iter (map type-tag args)))

(try-all-coercions (list n1 n2 complex))


  













