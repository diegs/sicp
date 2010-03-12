(define nil '())

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
