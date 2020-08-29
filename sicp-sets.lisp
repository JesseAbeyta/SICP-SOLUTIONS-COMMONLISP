(defun element-of-set (x set)
  (cond ((null set) nil)
	((equal x (car set)) t)
	(t (element-of-set x (cdr set)))))

(defun union-set (set1 set2)
  (cond
    ((null set1) set2)
    ((null set2) set1)
    ((element-of-set (car set1) set2) (union-set (cdr set1) set2))
    (t (union-set (cdr set1) (cons (car set1) set2)))))

(defun adjoin-set (x set)
  (cons x set))

(defun intersection-set (set1 set2)
  (cond ((or (null set1) (null set2) nil))
	((element-of-set (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(t (intersection-set (cdr set1) set2))))

(defun element-of-ordered-set (x set)
  (cond ((null set) nil)
	((= x (car set)) t)
	((< x (car set)) nil)
	(t (element-of-set x (cdr set)))))

(defun adjoin-ordered-set (x set)
  (defun adjoin-helper (x set acc)
    (cond ((null set) (cons x acc))
	  ((= x (car set)) (append acc set))
	  ((< x (car set)) (append acc (cons x set)))
	  (t (adjoin-helper x (cdr set) (append acc (list (car set)))))))
  (adjoin-helper x set nil))

;; Set2 should try to always have the biggest leading number
(defun union-ordered-set (set1 set2)
  (cond ((null set1) set2)
	((null set2) set1)
	((< (car set1) (car set2))
	 (cons (car set1) (union-ordered-set (cdr set1) set2)))
	((= (car set1) (car set2))
	 (cons (car set1) (union-ordered-set (cdr set1) (cdr set2))))
	(t
	 (cons (car set2) (cons (car set1) (union-ordered-set (cdr set1) (cdr set2)))))))

(defun lookup (key set-of-records)
  (let ((current-record (car set-of-records)))
    (cond ((null current-record) nil)
	  ((= key current-record) current-record)
	  ((< key current-record) (lookup key (cadr set-of-records)))
	  ((> key current-record) (lookup key (caddr set-of-records))))))
