;define nil for convenience
(define nil (quote ()))

(define odds (list 1 3 5 7))
(define squares (list 1 4 9 16 25))

;exercise 2.17 - last-pair procedure
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))
(last-pair (list 23 72 149 34))

;exercise 2.18 - reverse procedure
(define (reverse list)
  (define (reverse-iter list accum)
    (if (null? (cdr list))
	(cons (car list) accum)
	(reverse-iter (cdr list) (cons (car list) accum))))
  (reverse-iter list ()))
(reverse (list 1 4 9 16 25))

;exercise 2.19 - count-change redone using lists
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))

;the order of coin-values does not affect the result, but it may cause
;the problem space to be partitioned less efficiently, based on the
;recursive plan.

;exercise 2.20 - return numbers of same parity, arbitrary number of arguments
;note: this procedure computes parity of each element of equal parity in the
;list twice (except the first and last elements). it could be made more
;efficient by caching and reusing the parity of the first element.
(define (same-parity . x)
  (define (parity y)
    (modulo y 2))
  (define (same-parity-recur x)
    (cond ((null? (cdr x)) x)
	  ((= (parity (car x))
	      (parity (car (cdr x))))
	   (cons (car x) (same-parity-recur (cdr x))))
	  (else
	   (same-parity-recur (cons (car x) (cdr (cdr x)))))))
    (same-parity-recur x))

;exercise 2.21 - fill in square-list definitions
(define (square-list-1 items)
  (if (null? items)
      ()
      (cons (* (car items) (car items)) (square-list-1 (cdr items)))))
(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

;exercise 2.22 - why does iterative mess up ordering
;a) naive iterative approach always inserts current entry at head of
;   list, which means that it will be reversed
;b) if you flip the cons you'll end up with a backwards list in which
;   the links are in the car positions and the values are in the cdr positions

;exercse 2.23 - for-each, like map but doesn't save values
;note: must use cond instead of if-else because if-else requires return value
(define (for-each proc items)
  (cond ((null? items))
	(else (proc (car items))
	      (for-each proc (cdr items))))
  #t)

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;exercse 2.24 - example tree
;(list 1 (list 2 (list 3 4)))
;Value 11: (1 (2 (3 4)))
; (1 (2 (3 4))) => (2 (3 4)) => (3 4) => 4
; |                 |            |
; 1                 2            3
; tree interpretation is obvious (look at above at an angle)

;exercise 2.25 - car and cdrs to get 7 from each tree
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

(car (car (list (list 7))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

;exercise 2.26 - list manipulation examples
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;Value 18: (1 2 3 4 5 6)

(cons x y)
;Value 19: ((1 2 3) 4 5 6)

(list x y)
;Value 20: ((1 2 3) (4 5 6))

;exercise 2.27 - deep reverse
;loop invariant: put head onto the list and process what's next
(define (deep-reverse l)
  (define (deep-reverse-iter l accum)
    (cond ((null? (cdr l))
	   (if (list? (car l))
	       (cons (deep-reverse-iter (car l) ()) accum)
	       (cons (car l) accum)))
	  (else
	   (if (list? (car l))
	       (deep-reverse-iter (cdr l)
				  (cons (deep-reverse-iter (car l) ()) accum))
	       (deep-reverse-iter (cdr l)
				  (cons (car l) accum))))))
  (deep-reverse-iter l ()))

(define x (list (list 1 2) (list 3 4)))
(reverse x)
(deep-reverse x)

;cleaned up slightly
(define (deep-reverse l)
  (define (deep-reverse-iter l accum)
    (let ((left-action (if (list? (car l))
			   (lambda (l) (deep-reverse-iter (car l) ()))
			   (lambda (l) (car l)))))
      (cond ((null? (cdr l))
	     (cons (left-action l) accum))
	    (else
	     (deep-reverse-iter (cdr l)
				(cons (left-action l) accum))))))
  (deep-reverse-iter l ()))

;exercise 2.28 - fringe, returns leaves of tree flattened in left-to-right
(define x (list (list 1 2) (list 3 4)))

;depth first search right to left, building up tree from leaves
(define (fringe l)
  (define (fringe-inner l accum)
    (if (null? (cdr l))
	(if (list? (car l))
	    (fringe-inner (car l) accum)
	    (cons (car l) accum))
	(if (list? (car l))
	    (fringe-inner (car l) (fringe-inner (cdr l) accum))
	    (cons (car l) (fringe-inner (cdr l) accum)))))
  (fringe-inner l ()))

(fringe x)

;exercse 2.29 - binary mobile
;part a: makers and selectors
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (terminal? branch)
  (not (list? (branch-structure branch))))

;part b: total-weight of mobile
(define (total-weight mobile)
  (define (branch-weight branch)
    (if (terminal? branch)
	(branch-structure branch)
	(total-weight (branch-structure branch))))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define x (make-mobile (make-branch 2 (make-mobile (make-branch 3 5)
						       (make-branch 1 6)))
			   (make-branch 4 5)))
(total-weight x)

;part c: balanced mobile - is each branch's torque*weight balanced,
;and are its sub-mobiles balanced. this algorithm is exponential.
(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch)
       (if (terminal? branch)
	   (branch-structure branch)
	   (total-weight (branch-structure branch)))))
  (define (branch-balanced? branch)
    (if (terminal? branch)
	#t
	(balanced? (branch-structure branch))))
  (and (= (torque (left-branch mobile))
	  (torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile)))))

(define y (make-mobile (make-branch 2 3)
		       (make-branch 6 1)))

(define z (make-mobile (make-branch 2 (make-mobile (make-branch 2 3)
						   (make-branch 6 1)))
		       (make-branch 2 4)))

;part d: what if we changed representation of mobile to cons?
;would only need to change my selectors (only added terminal? to plug the
;last leak in my abstraction)

;exercise 2.30 - square-tree - squares all the elements in a tree
(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;exercise 2.31 - abstraction of square-tree to tree-map
(define (tree-map fun tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map fun sub-tree)
	     (fun sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;exercise 2.32 - subsets of a list
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

;exercise 2.33
(define (map p sequence)
  (accumulate (lambda (x y) ) nil sequence))