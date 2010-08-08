;; *Exercise 2.77:* Louis Reasoner tries to evaluate the expression
;; `(magnitude z)' where `z' is the object shown in *Note Figure
;; 2-24::.  To his surprise, instead of the answer 5 he gets an error
;; message from `apply-generic', saying there is no method for the
;; operation `magnitude' on the types `(complex)'.  He shows this
;; interaction to Alyssa P. Hacker, who says "The problem is that the
;; complex-number selectors were never defined for `complex' numbers,
;; just for `polar' and `rectangular' numbers.  All you have to do to
;; make this work is add the following to the `complex' package:"

;;      (put 'real-part '(complex) real-part)
;;      (put 'imag-part '(complex) imag-part)
;;      (put 'magnitude '(complex) magnitude)
;;      (put 'angle '(complex) angle)

;; Describe in detail why this works.  As an example, trace through
;; all the procedures called in evaluating the expression `(magnitude
;; z)' where `z' is the object shown in *Note Figure 2-24::.  In
;; particular, how many times is `apply-generic' invoked?  What
;; procedure is dispatched to in each case?

; this works because we defer the procedures to those defined for
; different complex number types. thanks to the double-tagging,
; it will dispatch to the correct rectangular or polar results and
; then return. in this case each call will call apply-generic twice.
; the first will dispatch to the 'complex definitions, and the second
; will dispatch to magnitude for 'rectangular or 'polar, respectively.

;; *Exercise 2.78:* The internal procedures in the `scheme-number'
;; package are essentially nothing more than calls to the primitive
;; procedures `+', `-', etc.  It was not possible to use the
;; primitives of the language directly because our type-tag system
;; requires that each data object have a type attached to it.  In
;; fact, however, all Lisp implementations do have a type system,
;; which they use internally.  Primitive predicates such as `symbol?'
;; and `number?'  determine whether data objects have particular
;; types.  Modify the definitions of `type-tag', `contents', and
;; `attach-tag' from section *Note 2-4-2:: so that our generic system
;; takes advantage of Scheme's internal type system.  That is to say,
;; the system should work as before except that ordinary numbers
;; should be represented simply as Scheme numbers rather than as
;; pairs whose `car' is the symbol `scheme-number'.

; use primitive tags where possible, else "thick" tags
(define (type-tag thing)
  (if (number? thing) 'scheme-number (car thing)))
(define (contents thing)
  (if (number? thing) thing (cdr thing)))
(define (make-tag thing tag)
  (if (number? thing) thing (cons tag thing)))
; then modify the scheme number package to not tag anything it computes

;; *Exercise 2.79:* Define a generic equality predicate `equ?' that
;; tests the equality of two numbers, and install it in the generic
;; arithmetic package.  This operation should work for ordinary
;; numbers, rational numbers, and complex numbers.
(define (equ? x y) (apply-generic 'equ? x y))
(define (install-equ-package)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (eq? (numer x) (numer y))
			  (eq? (denom x) (denom y)))))
  ;; note: i could also implement eq? at a lower level for the
  ;; relevant types.
  (put 'equ? '(complex complex)
       (lambda (x y) (and (eq? (real-part x) (real-part y))
			  (eq? (imag-part x) (imag-part y)))))
  'done)

;; *Exercise 2.80:* Define a generic predicate `=zero?' that tests if
;; its argument is zero, and install it in the generic arithmetic
;; package.  This operation should work for ordinary numbers, rational
;; numbers, and complex numbers.

; this is just like 2.79

;; note: skipped 2.5.2 exercises

;; note: skipped 2.5.3 exercises