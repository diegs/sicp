;Exercise 2.53.  What would the interpreter print in response to
;evaluating each of the following expressions?
(list 'a 'b 'c)
;(a b c)
(list (list 'george))
;((george))
(cdr '((x1 x2) (y1 y2)))
;((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;(y1 y2)
(pair? (car '(a short list)))
;#f
(memq 'red '((red shoes) (blue socks)))
;#f
(memq 'red '(red shoes blue socks))
;(red shoes blue socks)

;Exercise 2.54.  To be more precise, we can define equal? recursively
;in terms of the basic eq? equality of symbols by saying that a and b
;are equal? if they are both symbols and the symbols are eq?, or if
;they are both lists such that (car a) is equal? to (car b) and (cdr
;a) is equal? to (cdr b). Using this idea, implement equal? as a
;procedure
(define (equal? a b)
  (cond ((and (list? a) (list? b))
	 (or (and (null? a) (null? b))
	     (and (and (pair? a) (pair? b))
		  (equal? (car a) (car b))
		  (equal? (cdr a) (cdr b)))))
	((or (list? a) (list? b)) #f)
	(else (eq? a b))))
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

;Exercise 2.55.  Eva Lu Ator types to the interpreter the expression
;(car ''abracadabra) To her surprise, the interpreter prints back
;quote. Explain.

;she is quoting the list (quote abracadabra), so the car of that is quote

;symbolic differentiation
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	(else
	 (error "unknown expression type -- DERIV" exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

;Exercise 2.56 - add exponentiation support to deriv
;recall: d(u**n)/dx = nu**(n-1) * (du/dx)
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (make-exponentiation base exp)
  (cond ((= exp 0) 1)
	((= exp 1) base)
	(else (list '** base exp))))
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp) (- (exponent exp) 1)))
	  (deriv (base exp) var)))
	(else
	 (error "unknown expression type -- DERIV" exp))))

(deriv '(* 3 (** x -1)) 'x)
(deriv '(* 3 (** x 1)) 'x)
(deriv '(* 3 (** x 2)) 'x)
(deriv '(* 3 (** x 3)) 'x)

;Exercise 2.58: convert to infix operators
;(a) fully parenthesized infix form
(define (sum? x)
  (and (pair? x) (not (null? (cdr x))) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (not (null? (cdr x))) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	(else
	 (error "unknown expression type -- DERIV" exp))))
(sum? '(x + (3 * x)))
(deriv '(x + (3 * (x + (y + 2)))) 'x)

;b allow standard algebriac notation -- meh

;2.3.3: sets
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

;2.59: implement union-set for unordered list representation of sets
;recursive plan: only add things from set1 if they are *not* in set2.
;always add things from set 2
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (union-set (cdr set1) (cons (car set1) set2)))))

(union-set '(3 4 5) '(1 3 2))

;2.60: re-implement the four procedures for sets that allow duplicates.
;what is the impact on efficiency?

;element-of-set? is unchanged, still O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

;o(1) now
(define (adjoin-set x set)
  (cons x set))

;intersection: need to avoid duplicates  - adds o(n) factor to n^2 term
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((and (element-of-set? (car set1) set2)
	      (not (element-of-set? (car set1) (cdr set1)))) ;grab last occur. only
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(intersection-set '(1 2 3 2 1) '(2 3 3 1))

;union: need to avoid duplicates, then remove - adds o(n^2) factor on the end
(define (remove-duplicates-set set)
  (define (remove-duplicates-internal x accum)
    (cond ((null? x) accum)
	  ((element-of-set? (car x) (cdr x))
	   (remove-duplicates-internal (cdr x) accum))
	  (else (remove-duplicates-internal (cdr x) (cons (car x) accum)))))
  (remove-duplicates-internal set '()))

(define (union-set set1 set2)
  (cond ((null? set1) (remove-duplicates-set set2))
	((null? set2) (union-set '() set1))
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (union-set (cdr set1) (cons (car set1) set2)))))

(union-set '(3 4 3 5) '(1 3 3 2))
(union-set '(3 4 5) '(1 3 3 2))
(union-set '(3 4 3 5) '(1 3 2))

;sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (intersection-set (cdr set1)
				       (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set set1 (cdr set2)))))))

;Exercise 2.61: implement adjoin-set for ordered representation
;inserts it in the right place -- O(n/2)
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((= x (car set)) set)
	((> x (car set))
	 (cons (car set) (adjoin-set x (cdr set))))
	(else (cons x set))))
(adjoin-set 5 '(1 2 3 4 6))
(adjoin-set 5 '(1 2 3 4 5 6))
(adjoin-set 6 '(1 2 3 4 5))
(adjoin-set 1 '(2 3 4 5 6))

;Exercise 2.62: give O(n) implementation of union-set for ordered representations
;performs parallel walk of the two sets. O(n + m)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2)
		       (cons x1
			     (union-set (cdr set1)
					(cdr set2))))
		      ((< x1 x2)
		       (cons x1
			     (union-set (cdr set1)
					set2)))
		      ((< x2 x1)
		       (cons x2
			     (union-set set1
					(cdr set2)))))))))
(union-set '(1 3 5 7) '(2 4 6 8))

;sets as binary trees