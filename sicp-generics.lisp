(defun make-from-mag-ang (mag ang)
  (defun dispatch(op)
    (cond ((equal op 'real-part) (* mag (cos ang)))
	  ((equal op 'imag-part) (* mag (sin ang)))
	  ((equal op 'magnitude) mag)
	  ((equal op 'angle) ang)))
  #'dispatch)

(defun equ (x1 x2)
  (apply-generic 'equ x1 x2))

(put 'equ '(complex complex)
     (lambda (c1 c2)
       (and (= (real-part c1) (real-part c2))
	    (= (imag-part c1) (imag-part c2)))))

(put 'equ '(number number)
     (lambda (n1 n2) (= n1 n2)))

(put 'equ '(rational rational)
     (lambda (r1 r2)
       (and (= (numerator r1) (numerator r2))
	    (= (denominator r1) (denominator r2)))))

(defun =zero (x)
  (apply-generic '=zero x))

(put '=zero '(complex)
     (lambda (c1) (and (= (real-part c1) 0)
			  (= (real-part c2) 0))))

(put '=zero '(number)
     (lambda (x) (= 0 x)))

(put '=zero '(rational)
     (lambda (x) (= 0 (numerator x))))

(defun raise (x)
  (apply-generic 'raise x))

(put 'raise '(number)
     (lambda (x) (make-rat x 1)))
