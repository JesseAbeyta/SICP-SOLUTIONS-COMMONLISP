;; Exercise 3.1
(defun make-accumulator (val)
  (lambda (addend)
    (progn (setq val (+ val addend))
	   val)))

;; Exercise 3.2
(defun make-monitored (f)
  (let ((num_calls 0))
    (lambda (arg)
      (cond ((equal arg 'how-many-calls) num_calls)
	    ((equal arg 'reset-count) (setq num_calls 0))
	    (t (setq num_calls (+ num_calls 1))
	       (funcall f arg))))))

;; Exercise 3.3     
(defun make-account (balance password)
  (lambda (pw m)
    (flet ((withdraw (amount)
	     (if (>= balance amount)
		 (progn (setq balance (- balance amount))
			balance)
		 "Insufficient funds"))
	   (deposit (amount)
	     (setq balance (+ balance amount))
	     balance))
      (if (eql pw password)
	  (cond ((eql m 'withdraw) #'withdraw)
		((eql m 'deposit) #'deposit)
		(t "Unknown option"))
	  "Incorrect Password"))))

;; Exercise 3.4
(defun make-account (balance password)
  (let ((num-login-attempts 0))
    (lambda (pw m)
      (flet ((withdraw (amount)
	       (if (>= balance amount)
		   (progn (setq balance (- balance amount))
			  balance)
		   "Insufficient funds"))
	     (deposit (amount)
	       (setq balance (+ balance amount))
	       balance))
	(if (eql pw password)
	    (progn (setq num-login-attempts 0)
		   (cond ((eql m 'withdraw) #'withdraw)
			 ((eql m 'deposit) #'deposit)
			 (t "Unknown option")))
	    (progn
	      (setq num-login-attempts (+ 1 num-login-attempts))
	      (if (> num-login-attempts 7)
		  (call-the-cops))
	      "Incorrect Password"))))))

;; Exercise 3.5

;; Returns a random number between low and high
(defun rand-range (low high)
  (+ (random (- high low)) low))

;; Returns true if inside or on the unit circle
(defun example-p (x y)
  (if (<= (+ (* x x) (* y y)) 1)
      t
      nil))

(defun monte-carlo (trials experiment)
  (labels ((iter (trials-remaining trials-passed)
	     (cond ((= trials-remaining 0) (/ trials-passed trials))
		   ((funcall experiment) (iter (- trials-remaining 1)
					       (+ trials-passed 1)))
		   (t (funcall #'iter (- trials-remaining 1) trials-passed)))))
    (funcall #'iter trials 0)))

(defun estimate-integral (P x1 x2 y1 y2 num-trials)
  (flet ((experiment ()
	   (funcall P (rand-range x1 x2)
		      (rand-range y1 y2))))
    (* (monte-carlo num-trials #'experiment)
       (* (- x2 x1) (- y2 y1))))) ;; Multiply by area of rectangle

;; Exercise 3.6
(let ((x 4)) ;; 4 was randomly chosen :^)
  (defun my-rand (m)
    (cond ((eql m 'generate) (progn (setq x (+ 1 x))
				    x))
	  ((eql m 'reset) (lambda (new-val) (setq x new-val)))
	  (t "error unknown option"))))
