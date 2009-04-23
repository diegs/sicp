;exercise 1.9

;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))

;(+ 2 3)
;(inc (+ 1 3))
;(inc (inc (+ 0 3)))
;(inc (inc (3)))
;(inc 4)
;(5)
;=>recursive

;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))

;(+ 2 3)
;(+ 1 4)
;(+ 0 5)
;(5)
;=>iterative

;exercise 1.11
(define (f-recursive n)
  (if (< n 3) n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

(define (f-iterative n)
  (if (< n 3) n
      (f-iter n 3 2 1 0)))

(define (f-iter n i f1 f2 f3)
  (if (= i n) (f-apply f1 f2 f3)
      (f-iter n (+ i 1) (f-apply f1 f2 f3) f1 f2)))

(define (f-apply f1 f2 f3)
  (+ f1
     (* 2 f2)
     (* 3 f3)))

;exercise 1.12
;assumes valid x and y given
(define (pascal x y)
  (cond ((= x 1) 1)
	((= x y) 1)
	(else (+ (pascal (- x 1) (- y 1))
		 (pascal x (- y 1))))))

;exercise 1.16
(define (expt-iter b n)
  (expt-iter-step 1 b n))

(define (even? n)
  (= (remainder n 2) 0))

(define (expt-iter-step a b n)
  (cond ((= n 0) a)
	((even? n) (expt-iter-step a (* b b) (/ n 2)))
        (else (expt-iter-step (* a b) b (- n 1)))))

;exercise 1.17
(define (double n) (* n 2))
(define (half n) (/ n 2))

(define (times a b)
  (cond ((= b 0) 0)
	((even? b) (times (double a) (half b)))
	(else (+ a (times a (- b 1))))))

;exercise 1.18
(define (times-fast a b)
  (times-iter 0 a b))

(define (times-iter inv a b)
  (cond ((= b 0) inv)
	((even? b) (times-iter inv (double a) (half b)))
	(else (times-iter (+ inv a) a (- b 1)))))

;exercise 1.21
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
;(smallest-divisor 199)
;result: 199
;(smallest-divisor 1999)
;result: 1999
;(smallest-divisor 19999)
;result: 7

;exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond ((= (remainder start 2) 0) (search-for-primes (+ start 1) end))
        ((< start end) (timed-prime-test start)
		       (search-for-primes (+ start 2) end))))

;exercise 1.23
(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (smallest-divisor-new n)
  (find-divisor-new n 2))

(define (find-divisor-new n d)
  (cond ((> (* d d) n) n)
	((divides? d n) d)
	(else (find-divisor-new n (next d)))))

;(define (divides? d n)
;  (= (remainder n d) 0))




