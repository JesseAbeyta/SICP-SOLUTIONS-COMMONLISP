(defparameter *op-table* (make-hash-table :test #'equal))

(defun put-op (op type proc)
  (setf (gethash (list op type) *op-table*) proc))

(defun get-op (op type)
  (gethash (list op type) *op-table*))

(defparameter *coercion-table* (make-hash-table))

(defun put-coercion (type-from type-to proc)
  (setf (gethash (list type-from type-to) *coercion-table*) proc))

(defun get-coercion (type-from type-to)
  (car (gethash (list type-from type-to) *coercion-table*)))

(defun attach-tag (type-tag contents)
  (if (numberp contents)
      contents
      (cons type-tag contents)))

(defun type-tag (datum)
  (cond ((numberp datum) 'number)
	((consp datum) (car datum))
	(t (error "Bad tagged data"))))

(defun contents (datum)
  (cond ((numberp datum) datum)
	((consp datum) (cdr datum))
	(t (error "Bad tagged data"))))

(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (get-op op type-tags)))
      (if proc
	  (apply proc (mapcar #'contents args))
	  (error
	   "No method for these types ~S -- APPLY-GENERIC"
	   (list op type-tags))))))

(defun install-complex-package ()
  (defun make-from-real-imag (x y)
    (defun dispatch (op)
      (cond ((equal op 'real-part) x)
	    ((equal op 'imag-part) y)
	    ((equal op 'magnitude) (sqrt (+ (* x x) (* y y))))
	    ((equal op 'angle) (atan y x))
	    (t (error "Unknown op: MAKE-FROM-REAL-IMAG"))))
    #'dispatch)
  (defun make-from-mag-ang (mag ang)
    (defun dispatch (op)
      (cond ((equal op 'real-part) (* mag (cos ang)))
	    ((equal op 'imag-part) (* mag (sin ang)))
	    ((equal op 'magnitude) mag)
	    ((equal op 'angle) ang)
	    (t (error "Unknown op: MAKE-FROM-REAL-IMAG"))))
    #'dispatch)
  ;; internal procedures
  (defun add-complex (z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (defun sub-complex (z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (defun mul-complex (z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (defun div-complex (z1 z2)
        (make-from-mag-ang (/ (mangitude z1) (magnitude z2))
			   (- (angle z1) (angle z2))))
  ;; Interface to rest of system
  (defun tag (z) (attach-tag 'complex z))
  (put-op 'add '(complex complex)
	  (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put-op 'sub '(complex complex)
	  (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put-op 'div '(complex complex)
	  (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put-op 'mul '(complex complex)
	  (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put-op 'make-from-real-imag 'complex
	  (lambda (x y) (tag (make-from-real-imag x y))))
  (put-op 'make-from-mag-ang 'complex
	  (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(defun install-rational-package ()
  
