;definitions of rational data struct
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

