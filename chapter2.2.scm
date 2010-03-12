;define nil for convenience
(define nil (quote ()))

(define odds (list 1 3 5 7))
(define squares (list 1 4 9 16 25))

;exercise 2.17 - last-pair procedure
(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))
(last-pair (list 23 72 149 34))

;exercise 2.18 - reverse procedure
(define (reverse list)
  (define (reverse-iter list accum)
    (if (null? list)
	accum
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
(define (same-parity . x)
  (define (parity y)
    (modulo y 2))
  (define (same-parity-recur x p)
    (cond ((null? x) x)
	  ((= p (parity (car x)))
	   (cons (car x) (same-parity-recur (cdr x) p)))
	  (else (same-parity-recur (cdr x) p))))
  (same-parity-recur x (parity (car x))))
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;exercise 2.21 - fill in square-list definitions
(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list-1 (cdr items)))))
(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))
(square-list-1 (list 1 2 3 4 5))
(square-list-2 (list 1 2 3 4 5))

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
(define x (list 1 (list 2 (list 3 4))))
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
(define (deep-reverse l)
  (define (deep-reverse-iter l accum)
    (let ((left (cons (if (pair? (car l))
			  (deep-reverse-iter (car l) ())
			  (car l))
		      accum)))
      (if (pair? (cdr l))
	  (deep-reverse-iter (cdr l) left)
	  left)))
  (deep-reverse-iter l ()))

(define x (list (list 1 2) (list 3 4)))
(reverse x)
(deep-reverse x)

;exercise 2.28 - fringe, returns leaves of tree flattened in left-to-right
(define (fringe l)
  (define (fringe-inner l accum)
    (let ((right (if (pair? (cdr l))
		     (fringe-inner (cdr l) accum)
		     accum)))
      (if (pair? (car l))
	  (fringe-inner (car l) right)
	  (cons (car l) right))))
  (fringe-inner l ()))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))

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

(define x (make-mobile (make-branch 2
				    (make-mobile (make-branch 3 5)
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
       (branch-balanced? (right-branch mobile))))

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

;common interfaces
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;exercise 2.33 - define primitives in terms of common interfaces
;; (define (map p sequence)
;;   (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
;; (map square (list 1 2 3 4 5))

;; (define (append seq1 seq2)
;;   (accumulate cons seq2 seq1))
;; (append (list 1 2 3) (list 4 5 6))

;; (define (length sequence)
;;   (accumulate (lambda (x y) (+ y 1)) 0 sequence))
;; (length (list 1 2 3 4 5))

;exercise 2.34 - horner's rule
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff
		   (* x
		      (if (pair? higher-terms)
			  (horner-eval x higher-terms)
			  higher-terms))))
              0
              coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

;exercise 2.35 count-leaves as accumulation
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
			   (if (pair? x)
			       (count-leaves x)
			       1)) t)))
(define x (list (list 1 2) (list 3 4)))
(count-leaves x)

;exercise 2.36 - accumulate-n accumulates sequence of sequences
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))
(accumulate-n + 0 (list (list 1 2 3)
			(list 4 5 6)
			(list 7 8 9)
			(list 10 11 12)))

;exercise 2.37 - matrices
;this should check for proper dimensionality first
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(define (transpose mat)
  (accumulate-n cons () mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (map (lambda (y) (dot-product x y)) cols)) m)))

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define v (list 6 4 7 2))
(dot-product v v)
(matrix-*-vector m v)
(transpose m)
(matrix-*-matrix m m)

;exercise 2.38 - foldl and foldr
;; (define (fold-left op initial sequence)
;;   (define (iter result rest)
;;     (if (null? rest)
;;         result
;;         (iter (op result (car rest))
;;               (cdr rest))))
;;   (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
;commutivity is the property of op to satisfy foldr = foldl

;exercise 2.39 - reverse in terms of foldr and foldl
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(reverse (list 1 2 3 4 5))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
(reverse (list 1 2 3 4 5))

;exercise 2.40 - unique-pairs sucht that 1<=j<i<=n
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (x) (map (lambda (y) (list x y))
			    (enumerate-interval 1 (- x 1))))
	   (enumerate-interval 1 n)))
(unique-pairs 5)

;; (define (prime-sum-pairs n)
;;   (map make-pair-sum
;;        (filter prime-sum?
;; 	       (unique-pairs n))))
;; (prime-sum-pairs 5)

;exercise 2.41 - find triples of distinct ordered i,j,k less than or equal to n
;that sum to given integer s
(define (find-triples-with-sum n s)
  (define (make-triple-sum triple)
    (append triple (list (fold-right + 0 triple))))
  (define (sums-to-s? triple)
    (= s (fold-right + 0 triple)))
  (define (unique-triples n)
    (flatmap (lambda (z) (flatmap
			  (lambda (y) (map (lambda (x) (list x y z))
					   (enumerate-interval 1 (- y 1))))
			  (enumerate-interval 1 (- z 1))))
	     (enumerate-interval 1 n)))
  (map make-triple-sum
       (filter sums-to-s?
	       (unique-triples n))))
(find-triples-with-sum 20 14)

;exercise 2.42 - k queens problem
(define (queens board-size)
  (define empty-board nil)
  (define (adjoin-position row k others)
    (append others (list (cons k row))))
  (define (remove item sequence)
    (filter (lambda (x) (not (= (car x) (car item))))
	    sequence))
  (define (safe? k positions)
    (let ((kth (car (filter (lambda (x) (= (car x) k)) positions))))
      (fold-right (lambda (position accum)
		    (and accum (not (colinear? position kth))))
		  #t
		  (remove kth positions))))
  (define (colinear? position k)
    (or (= (cdr position) (cdr k))
	(= (abs (- (cdr k) (cdr position)))
	   (abs (- (car k) (car position))))))
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;exercise 2.43 - slow queens
;this is much slower because it re-solves the k-1 case
;for each row attempted (which in turn, does the same).
;(board-size)^(board-size) time instead of board-size time
