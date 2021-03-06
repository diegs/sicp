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
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

;Exercise 2.63: compare two tree->list procedures
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

;(a) do the two procedures produce the same result for every tree?
;
;the first creates a list of in-order tree traversal.  the second also
;creates a list of in-order tree traversal.
(define make-test-tree
  (adjoin-set 1
	      (adjoin-set 2
			  (adjoin-set 4
				      (adjoin-set 3
						  (adjoin-set 7 '()))))))
(tree->list-1 make-test-tree)
(tree->list-2 make-test-tree)

;(b) do the two procedures have the same order of growth for a
;balanced tree?
;
;the first each right tree node is appended onto the left tree,
;which is o(n), so the result is O(n^2). whereas the second builds up
;the result only once, right to left, which is O(n).

;Exercise 2.64: questions about the following partial-tree procedure:
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

;(a) explain how partial-tree works. draw tree for '(1 3 5 7 9 11)
;
;it recursively builds up the left tree, the center node, and the right tree
;by partitioning the list into three pieces. based on the way it divides
;the left and right portions, it's slightly "right-tilted":
;
;      5
;    /   \
;   1     9
;  / \   / \
; ()  3 7  11

;(b) what is the order growth for list->tree?
;
;(1) length elements is O(n)
;(2) at each step, we build a left result and right result and then we make-tree
;    them, which is O(3)
;(3) we build n/2 such nodes (since n/2 nodes are leaves), which leads me to believe
;    that this whole thing is O(3n/2). this is because the list is ordered.

;Exercise 2.65: implement union-set and intersection-set for sets implemented as
;(balanced) binary trees.
;
;not going to implement because i have to go back and rename stuff, but my strategy
;is to use O(n) list->tree and tree->list operators, combined with
;O(n) union/intersect-sets-of-ordered-lists operators, to get what i want.

;Exercise 2.66: implement lookup procedure for where the case is structured
;as a binary tree ordered by the numerical value of the keys
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (entry set-of-records)))
	 (entry set-of-records))
	((< given-key (key (entry set-of-records)))
	 (lookup given-key (left-tree set-of-records)))
	(else (lookup given-key (right-tree set-of-records)))))

;2.3.4 Example: Huffman Encoding Trees
(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)   ; symbol
			       (cadr pair)) ; frequency
		    (make-leaf-set (cdr pairs))))))

;Exercise 2.67: Define an encoding tree and a sample message
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;Value 52: (a d a b b c a)

;*Exercise 2.68:* The `encode' procedure takes as arguments a
;message and a tree and produces the list of bits that gives the
;encoded message.

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (contains? elem list)
  (cond ((null? list) #f)
	((eq? elem (car list)) #t)
	(else (contains elem (cdr list)))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) (if (eq? symbol (symbol-leaf tree)) '()
			    (error "symbol not in tree!")))
	  ((contains? symbol (symbols (left-branch tree)))
	   (cons 0 (encode-symbol symbol (left-branch tree))))
	  ((contains? symbol (symbols (right-branch tree)))
	   (cons 1 (encode-symbol symbol (right-branch tree))))
	  (else (error "symbol not in tree!"))))

(encode '(a d a b b c a) sample-tree)
;Value 53: (0 1 1 0 0 1 0 1 0 1 1 1 0)

;; *Exercise 2.69:* The following procedure takes as its argument a
;; list of symbol-frequency pairs (where no symbol appears in more
;; than one pair) and generates a Huffman encoding tree according to
;; the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
	((null? (cdr leaf-set)) (car leaf-set))
	(else (let ((left (car leaf-set))
		    (right (cadr leaf-set))
		    (rest (cddr leaf-set)))
		(successive-merge (adjoin-set
				   (make-code-tree left right)
				   rest))))))

;; *Exercise 2.70:* Use `generate-huffman-tree' (*Note Exercise 2-69::)
;; to generate a corresponding Huffman tree, and use `encode' (*Note

;; Exercise 2-68::) to encode the following message

(define fifties-tree (generate-huffman-tree
		      (list '(A 2) '(BOOM 1) '(GET 2) '(JOB 2)
			    '(NA 16) '(SHA 3) '(YIP 9) '(WAH 1))))

(define fifties-msg (encode '(Get a job
				  Sha na na na na na na na na
				  Get a job
				  Sha na na na na na na na na
				  Wah yip yip yip yip yip yip yip yip yip
				  Sha boom)
			    fifties-tree))

(decode fifties-msg fifties-tree)

;; *Exercise 2.71:* Suppose we have a Huffman tree for an alphabet of
;; n symbols, and that the relative frequencies of the symbols are 1,
;; 2, 4, ..., 2^(n-1).  Sketch the tree for n=5; for n=10.  In such a
;; tree (for general n) how may bits are required to encode the most
;; frequent symbol?  the least frequent symbol?

; (skipped)

;; *Exercise 2.71:* Suppose we have a Huffman tree for an alphabet of
;; n symbols, and that the relative frequencies of the symbols are 1,
;; 2, 4, ..., 2^(n-1).  Sketch the tree for n=5; for n=10.  In such a
;; tree (for general n) how may bits are required to encode the most
;; frequent symbol?  the least frequent symbol?

; (skipped)
