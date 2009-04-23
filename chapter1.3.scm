;exercise 1.29 - simpson's rule
(define (sum a func iter b)
  (if (> a b)
      0
      (+ (func a) (sum (iter a) func iter b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (iter a) (+ a (* 2 h)))
  (* (/ h 3) (+ (f a)
		(f b)
		(* 4 (sum (+ a h) f iter (- b h)))
		(* 2 (sum (+ a h h) f iter (- b h))))))

;exercise 1.30 - iterative sum
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

;(define (identity x) x)
;(define (inc a) (+ a 1))

;exercise 1.31 - product
;recursive
(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a) (product-recursive term (next a) next b))))

;iterative
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

;factorial
(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product-recursive identity 1 inc n))

;pi approximation: pi/4 ~= 2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7...
(define (pi-approx n)
  (define (term a) (* (/ a (+ a 1)) (/ (+ a 2) (+ a 1))))
  (define (next a) (+ a 2))
  (* 4. (product-iter term 2 next n)))

;exercise 1.32 - accumulator
;recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

;Iterative
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
    (iter a null-value))

;sum as accumulate
(define (sum-accumulate term a next b)
  (define (combiner a b) (+ a b))
  (accumulate-iter combiner 0 term a next b))

;product as accumulate
(define (prod-accumulate term a next b)
  (define (combiner a b) (* a b))
  (accumulate-iter combiner 1 term a next b))

;exercise 1.33 - filtered accumulate
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (combiner result
			(if (filter (term a)) (term a) null-value)))))
  (iter a null-value))

;sum of squares of prime numbers
(define (sos-prime a b)
  (define (next a) (+ a 1))
  (define (term a) (* a a))
  (define (filter a) (prime? a))
  (define (combiner a b) (+ a b))
  (filtered-accumulate combiner 0 term a next b filter))

;product of all positive integers less than n relatively prime to n
(define (prod-rel-prime n)
  (define (next a) (+ a 1))
  (define (term a) (a))
  (define (filter a) (= (gcd a n) 1))
  (define (combiner a b) (* a b))
  (filtered-accumulate combiner 1 term 1 next n filter))

;fixed-point procedures from the text
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;exercise 1.35 - golden ratio
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
	       1.0))

;exercise 1.36 - modified fixed-point
(define (fixed-point-mod f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess iter)
    (let ((next (f guess)))
      (display "Iteration: ")
      (display iter)
      (display " Value: ")
      (display next)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next (+ iter 1)))))
  (try first-guess 0))
  
(define (xtox)
  (fixed-point-mod (lambda (x) (/ (log 1000) (log x)))
		   2.0))

(define (average a b) (/ (+ a b) 2))
(define (xtox-avg)
  (fixed-point-mod (lambda (x) (average x
					(/ (log 1000) (log x))))
		   2.0))

;exercise 1.37 - cont-frac
;iterative
(define (cont-frac-iter n d k)
  (define (cont-frac-iter-step n d k i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac-iter-step n d k (+ i 1))))))
  (cont-frac-iter-step n d k 1))

;recursive
(define (cont-frac-recursive n d k)
  (define (cont-frac-recursive-step n d k result)
    (if (= k 0)
	result
	(cont-frac-recursive-step n d (- k 1) (/ (n k) (+ (d k) result)))))
  (cont-frac-recursive-step n d (- k 1) (/ (n k) (d k))))

;test procedure
(define (test-cont-frac cont-frac start end)
  (display (cont-frac (lambda (i) 1.0)
		       (lambda (i) 1.0)
		       start))
  (newline)
  (if (< start end)
       (test-cont-frac cont-frac (+ start 1) end)))

;exercise 1.38 - euler's De Fractionibus Continuis
;approximates e ~= 2 + cont-frac where n = 1 and d = 1,2,1,1,4,1,1,6...
(define (euler-continuis k)
  (+ 2 (cont-frac-iter (lambda (i) 1.0)
		       (lambda (i) (if (= (modulo (+ i 1) 3) 0)
				       (* (/ (+ i 1) 3) 2.0)
				       1.0))
		       k)))

;exercise 1.39 - tan c-f where n=x,-x^2,-x^2... and d = 1,3,5...
;note: ungraceful handling of (= x 0)
(define (tan-cf x k)
  (/ (cont-frac-iter (lambda (i) (- (* x x)))
		     (lambda (i) (- (* i 2) 1.0))
		     k)
     (- x)))

;procedures for section 1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))

;(define (sqrt x)
;  (fixed-point (average-damp (lambda (y) (/ x y)))
;               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;exercise 1.40 - zeros of cubic using newton's method
(define (cubic-zeros a b c)
  (newtons-method (cubic a b c) 1))

(define (cubic a b c)
  (lambda (x) (+ (expt x 3)
		 (* a (expt x 2))
		 (* b x)
		 c)))

;exercise 1.41 - returns procedure that applies input procedure twice
(define (double f)
  (lambda (x) (f (f x))))

;exercise 1.42 - f(g(x))
(define (compose f g)
  (lambda (x) (f (g x))))

;exericse 1.43 - nth repeated application of f
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;exercise 1.44 - smoothing
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx)))
		 3)))

;smooth the smoothed function
(define (smooth-n-fold f n)
  ((repeated smooth n) f))

;exercise 1.45 - repeated average damps for fixed points of higher powers
(define (nth-root x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
			    (repeated average-damp (- n 1))
			    1.0))

;exercise 1.46 - generalized iterative improvement computation
;returns procedure that takes guess as argument and keeps improving
;guess until it is good enough
(define (iterative-improvement good-enough improve)
  (lambda (guess)
    (define (seek guess)
      (let ((new-guess (improve guess)))
	(if (good-enough guess new-guess)
	    guess
	    (seek new-guess))))
      (seek guess)))

(define (good-enough x y)
  (< (abs (- x y)) 0.000001))

;sqrt in terms of iterative improvement
(define (sqrt-ii x)
  ((iterative-improvement good-enough
			  (lambda (y) (average y (/ x y))))
  1.0))

;fixed point in terms of iterative improvement
(define (fixed-point-ii f)
  ((iterative-improvement good-enough
			  (lambda (x) (average x (f x))))
   1.0))