(define (variable? x) (symbol? x))

(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2) 
	((=number? a2 0) a1) 
	((and (number? a1) (number? a2)) (+ a1 a2)) 
	(else (list '+ a1 a2))))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
	((=number? m1 1) m2) ((=number? m2 1) m1) 
	((and (number? m1) (number? m2)) (* m1 m2)) 
	(else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) 
  (cadr s))

(define (augend s)
  #|(if (= (size s) 3)
      (caddr s)
      (append (list '+) (cddr s))))|#
  (fold-right make-sum 0 (cddr s)))

(augend '(+ (* 2 x) (** x 3) 5 (*3 x)))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (exponentiation? exp)
  (and (pair? exp)
       (eq? (car exp) '**)))

(define (make-exponentiation base exponent)
  (cond ((= exponent 1) base)
	((= exponent 0) 1)
	(else (list '** base exponent))))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (deriv exp var) 
  (cond ((number? exp) 0) 
	((variable? exp) (if (same-variable? exp var) 1 0)) 
	((sum? exp) (print exp) (make-sum (deriv (addend exp) var) 
			      (deriv (augend exp) var))) 
	((product? exp) (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var)) 
				  (make-product (deriv (multiplier exp) var) (multiplicand exp)))) 
	((exponentiation? exp) (make-product (exponent exp) 
					     (make-exponentiation (base exp) (- (exponent exp) 1))))
	(else (error "unknown expression type: DERIV" exp))))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* a (** x 2)) 'x)

(deriv '(+ x (* 2 x) (** x 3)) 'x)

(define (infix->prefix exp)
  (cond ((null? exp) nil)
	((and (not (pair? (car exp))) (= (length exp) 1)) (car exp))
	((pair? (car exp))
		(append (list (cadar exp) (caar exp))
			(list (infix->prefix (cddar exp)))))
	(else (append (list (cadr exp) (car exp))
		      (list (infix->prefix (cddr exp)))))))

(infix->prefix '(x * 3 + 2))

(deriv (infix->prefix '(x + 3 * (x + y + 2))) 'x)









