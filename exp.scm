(define (raise x y)
  (define (iter y result)
    (if (= y 0)
	result
	(iter (- y 1) (* x result))))
  (iter y 1))

(raise 2 10)