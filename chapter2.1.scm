;definitions of rational data structure
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;testing values
(define one-third (make-rat 1 3))
(define one-half (make-rat 1 2))

;primitive operations
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;better make-rat that reduces num and denom to lowest terms
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g)))) 

;exercise 2.1 - make-rat that handles positive and negative arguments
(define (sign x) (if (< x 0) -1 1))
(define (make-rat n d)
  (let ((s (* (sign n) (sign d)))
	(g (gcd (abs n) (abs d))))
    (cons (* s (/ (abs n) g)) (/ (abs d) g))))

;exercise 2.2 - abstraction for a line segment made up of points
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (average x y) (/ (+ x y) 2))
(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s))
		       (x-point (end-segment s)))
	      (average (y-point (start-segment s))
		       (y-point (end-segment s)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-segment s)
  (print-point (start-segment s))
  (display "-")
  (print-point (end-segment s)))

;exercise 2.3 - rectangles in a plane
(define (make-rect len wid)
  (cons (make-segment (make-point 0 0)
		      (make-point 0 len))
	(make-segment (make-point 0 0)
		      (make-point 0 wid))))
(define (len rect) (car rect))
(define (wid rect) (cdr rect))

(define (print-rect rect)
  (newline)
  (display "(")
  (print-segment (len rect))
  (display ")x(")
  (print-segment (wid rect))
  (display ")"))

(define (segment-length s)
  (sqrt (+ (square (- (x-point (start-segment s))
		      (x-point (end-segment s))))
	   (square (- (y-point (start-segment s))
		      (y-point (end-segment s)))))))

(define (perimeter rect)
  (+ (* 2 (segment-length (len rect)))
     (* 2 (segment-length (wid rect)))))

(define (area rect)
  (* (segment-length (len rect))
     (segment-length (wid rect))))

;alternative version: uses three points. area and perim should work
(define (make-rect x y z)
  (cons x (cons y z)))

(define (len rect)
  (make-segment (rect-x rect) (rect-y rect)))
(define (wid rect)
  (make-segment (rect-x rect) (rect-z rect)))
(define (rect-x rect) (car rect))
(define (rect-y rect) (car (cdr rect)))
(define (rect-z rect) (cdr (cdr rect)))

;exercise 2.4 - another representation of pairs
;;(define(cons xy)
;;  (lambda (m) (m x y)))

;; (define(car z)
;;   (z (lambda (pq) p)))

;; (define(cdr z)
;;   (z (lambda (pq) q)))

;exercise 2.5 - representing pairs as 2^a 3^b
;; (define (cons x y)
;;   (* (expt 2 x) (expt 3 y)))

;; (define (expt-helper n factor accum)
;;   (if (= (modulo n factor) 0)
;;       (expt-helper (/ n factor) factor (+ accum 1))
;;       accum))

;; (define (car z)
;;   (expt-helper z 2 0))

;; (define (cdr z)
;;   (expt-helper z 3 0))

;exercise 2.6 - church numerals - definition of 0 and add-1 (given)
;; (define zero (lambda (f) (lambda (x) x)))
;; (define (add-1 n)
;;   (lambda (f) (lambda (x) (f ((n f) x)))))

;; ;showing the definition of 1 by substitution
;; (add-1 (lambda (g) (lambda (y) y)))
;; (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x))))
;; (lambda (f) (lambda (x) (f x)))

;; ; explicit definition of 1, 2 and +
;; (define one (lambda (f) (lambda (x) (f x))))
;; (define two (lambda (f) (lambda (x) (f (f x)))))
;; (define + (m n) (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;exercise 2.7 - interval abstraction
(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

;exercise 2.8 - subtraction
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
				 (- (lower-bound y)))))

;exercise 2.9 - width of interval
;1. width of sum of interval:
;     (y - x) / 2 + (v - w) / 2
;     1/2 ( y - x + v - w)
;   equals
;     1/2 ((y + v) - (x + w))
;2. width of difference of interval
;     (trivial)
;3. not true for multiplication or (division)
;   i.e.
;     (3, 5)  (8, 10)
;   interval first
;     (1 * 2) = 1
;   multiply first
;     interval(24, 40) => 8

;exercise 2.10 - error handling for divide by 0
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (< (lower-bound y) 0)
	   (> (upper-bound y)) 0)
      (error "cannot divide by span that crosses 0")
      (mul-interval x 
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

;exercise 2.11 - alternative multiplication
;|lb x|ub x|lb y|ub y|lb x*y|ub x*y|
;|< 0 |< 0 |< 0 |< 0 |ux*uy |lx*ly | uniq
;|< 0 |< 0 |< 0 |> 0 |lx*uy |ux*ly | x2
;|< 0 |< 0 |> 0 |> 0 |lx*uy |ux*ly | x2
;|< 0 |> 0 |< 0 |> 0 | uniq
;|< 0 |> 0 |> 0 |> 0 | x2
;|> 0 |> 0 |> 0 |> 0 | uniq
;;(define (mul-interval x y)
;;   (cond ((and (< (lower-bound x) 0) (< (upper-bound x) 0))
;; 	 (cond ((and (< (lower-bound y) 0) (< (upper-bound y) 0))
;; 		(make-interval (* (upper-bound x) (upper-bound y))
;; 			       (* (lower-bound x) (lower-bound y))))
;; 	       ((and (< (lower-bound y) 0) (> (upper-bound y) 0))
;; 		(make-interval (* (lower-bound x) (upper-bound y))
;; 			       (* (upper-bound x) (lower-bound y))))
;; 	       (else (make-interval (* (lower-bound x) (upper-bound y))
;; 				    (* (upper-bound x) (lower-bound y)))))
;; 	 ((and (< (lower-bound x) 0) (> (upper-bound x) 0))
;; 	  (cond ((and (< (lower-bound y) 0) (> upper-bound y) 0)
;; 		 (let ((p1 (* (lower-bound x) (lower-bound y)))
;; 		       (p2 (* (lower-bound x) (upper-bound y)))
;; 		       (p3 (* (upper-bound x) (lower-bound y)))
;; 		       (p4 (* (upper-bound x) (upper-bound y))))
;; 		   (make-interval (min p1 p2 p3 p4)
;; 				  (max p1 p2 p3 p4)))))))))
		   
;exercise 2.12 - add percent tolerance
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c tol)
  (make-center-width c (make-width c tol)))
(define (make-width c tol)
  (* c (/ tol 100)))
(define (percent i)
  (* (/ (width i) (center i)) 100))

;exercise 2.13 - under assumption of small percentage tolerances what is formula
;approximately a+b

;exercise 2.14 - parallel resistors, different results shown
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define a (make-center-percent 5 1))
(define b (make-center-percent 10 3))
(define c (make-center-percent 8 2))

(define (test-interval str int)
  (display str)
  (display ": center=")
  (display (center int))
  (display ", percent=")
  (display (percent int))
  (newline))

(test-interval "a" a)
(test-interval "b" b)
(test-interval "a/a" (div-interval a a))
(test-interval "a/b" (div-interval a b))
(test-interval "a/c" (div-interval a c))
(test-interval "b/c" (div-interval b c))
(test-interval "b/a" (div-interval b a))
(test-interval "c/a" (div-interval c a))
(test-interval "c/b" (div-interval c b))

;exercise 2.14 - why are fewer repetitions of algebraic operators "better"
;
;based on the above results, in the division of two intervals the percent
;tolerance is approximately added. this means that more operations means
;more expansion of tolerance, so it's less "tight"

;exercise 2.15