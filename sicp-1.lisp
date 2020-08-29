(defun dub (x)
  (* 2 x))

(defun half (x)
  (/ x 2))

(defun mult (a b)
  (defun mult-iter (x b sum)
    (cond ((= b 0) sum)
	  ((integerp (half b))
	   (mult-iter (dub x) (half b) sum))
	  (t (mult-iter x (- b 1) (+ x sum)))))
  (mult-iter a b 0))
