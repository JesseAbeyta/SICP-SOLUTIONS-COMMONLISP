;; Exercise 3.1
(defun make-accumulator (val)
  (lambda (addend)
    (progn (setq val (+ val addend))
	   val)))

;; Exercise 3.2
(defun make-monitored (f)
  (let ((num_calls 0))
    (defun dispatch (arg)
      (cond ((equal arg 'how-many-calls) num_calls)
	    ((equal arg 'reset-count) (setq num_calls 0))
	    (t (setq num_calls (+ num_calls 1))
	       (funcall f arg))))))

;; Exercise 3.3
(defun make-account (balance password)
  (defun withdraw (amount)
    (if (>= balance amount)
	(progn (setq balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (defun deposit (amount)
    (setq balance (+ balance amount))
    balance)
  (defun dispatch (m)
    (cond ((eql m 'withdraw
      
