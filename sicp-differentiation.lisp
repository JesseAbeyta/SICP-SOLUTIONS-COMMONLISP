
(defun =numberp (expr num)
  (and (numberp expr) (= expr num)))

(defun variablep (x) (symbolp x))
(defun same-variablep (x y)
    (and (variablep x) (variablep y) (eql x y)))

(defun make-sum (&rest nums)
  (append (list '+) nums))

(defun make-product (&rest nums)
  (append (list '*) nums))

(defun make-exponentiation (a b)
  (cond ((=numberp b 1) a)
	((=numberp b 0) 1)
	((and (numberp a) (numberp b)) (expt a b))
	(t (list '** a b))))

(defun exponentiationp (exp) (and (consp exp) (eql (car exp) '**)))
(defun base (exp) (cadr exp))
(defun exponent (exp) (caddr exp))

(defun sump (exp) (and (consp exp) (eql (car exp) '+)))
(defun addend (s) (cadr s))
(defun augend (s)
  (cond
    ((null (cdr s)) (car s))
    (t (append (list '+) (cddr s)))

(defun productp (exp) (and (consp exp) (eql (car exp) '*)))
(defun multiplier (p) (cadr p))
(defun multiplicand (p)
  (cond
    ((null (cdr p)) (car p))
    (t (append (list '*) (cddr p)))))

(defun deriv (exp var)
  (cond ((numberp exp) 0)
	((variablep exp) (if (same-variablep exp var) 1 0))
	((sump exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var)))
	((productp exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiationp exp)
	 (make-product
	  (make-product (exponent exp)
		       (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
	  (deriv (base exp) var)))
	(t
	 (error "unknown expression type"))))
	 
				
