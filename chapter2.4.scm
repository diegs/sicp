;; *Exercise 2.73:* Section *Note 2-3-2:: described a program
;; that performs symbolic differentiation:
;; a. Explain what was done above.  Why can't we assimilate the
;;    predicates `number?' and `same-variable?' into the
;;    data-directed dispatch?

; because it would recurse infinitely; those are the base cases

;; b. Write the procedures for derivatives of sums and products,
;;    and the auxiliary code required to install them in the table
;;    used by the program above.

(define (install-deriv-package)
  ;; internal procedures
  (define (sum exp var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
  (define (product exp var)
    (make-sum
     (make-product (multiplier exp)
		   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
		   (multiplicand exp))))

  ;; interface to the rest of the system
  (put 'deriv '(+) sum)
  (put 'deriv '(*) product))

;; c. Choose any additional differentiation rule that you like,
;;    such as the one for exponents (*Note Exercise 2-56::), and
;;    install it in this data-directed system.

(define (install-exponent-rule)
  (define (exponentiation exp var)
    (make-product
     (make-product (exponent exp)
		   (make-exponentiation (base exp) (- (exponent exp) 1)))
     (deriv (base exp) var)))
  ;; interface to rest of system
  (put 'deriv '(^) exponentiation))

;; d. In this simple algebraic manipulator the type of an
;;    expression is the algebraic operator that binds it together.
;;    Suppose, however, we indexed the procedures in the opposite
;;    way, so that the dispatch line in `deriv' looked like
;;         ((get (operator exp) 'deriv) (operands exp) var)
;;    What corresponding changes to the derivative system are
;;    required?

; simply flip the puts around so they look like this:
; (put '+ 'deriv sum)
; this will return the function that takes the operands and var
; and computes its derivative

;;*Exercise 2.74:* Insatiable Enterprises, Inc.
;; a. Implement for headquarters a `get-record' procedure that
;;    retrieves a specified employee's record from a specified
;;    personnel file.  The procedure should be applicable to any
;;    division's file.  Explain how the individual divisions' files
;;    should be structured.  In particular, what type information
;;    must be supplied?

; the file will need to provide a data type tag that corresponds
; to a tag that is supported by the procedures. therefore the
; procedure will first read the type tag from the file, and then
; invoke the appropriate procedure to fetch the record

;; b. Implement for headquarters a `get-salary' procedure that
;;    returns the salary information from a given employee's record
;;    from any division's personnel file.  How should the record be
;;    structured in order to make this operation work?

; the record should be tagged with a record type, so that the
; proper procedure can be dispatched

;; c. Implement for headquarters a `find-employee-record'
;;    procedure.  This should search all the divisions' files for
;;    the record of a given employee and return the record.  Assume
;;    that this procedure takes as arguments an employee's name and
;;    a list of all the divisions' files.

; simple - just opens all the files, reads their tags, calls
; find-employee-record with that tag, and returns the results

;; d. When Insatiable takes over a new company, what changes must
;;    be made in order to incorporate the new personnel information
;;    into the central system?

; need to tag the file and all the records with the appropriate tags,
; and then implement the procedures for those tags and add them to
; the table.

;; *Exercise 2.75:* Implement the constructor `make-from-mag-ang' in
;; message-passing style.  This procedure should be analogous to the
;; `make-from-real-imag' procedure given above.

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  ((eq? op 'real-part)
	   (* r (cos a)))
	  ((eq? op 'imag-part)
	   (* r (sin a)))
	  (else
	   (error "Unknown op -- MAKE-FROM-MAG-ANG"))))
  dispatch)

;; *Exercise 2.76:* As a large system with generic operations
;; evolves, new types of data objects or new operations may be needed.
;; For each of the three strategies--generic operations with explicit
;; dispatch, data-directed style, and message-passing-style--describe
;; the changes that must be made to a system in order to add new
;; types or new operations.  Which organization would be most
;; appropriate for a system in which new types must often be added?
;; Which would be most appropriate for a system in which new
;; operations must often be added?

; for an explicit dispatch, everything needs to be changed every time.

; for a data-directed style, all the table insertion code needs to be
; changed when you add a new procedure. nothing needs to be changed
; when you add a new type.

; for message-passing style, each dispatch creator needs to be
; changed when you add a new procedure. nothing needs to be changed
; when you add a new type.

; for a system where new types are added freqently, data-directed seems
; a little cleaner. for a system where new procedures are added, message-passing
; seems a little easier.

