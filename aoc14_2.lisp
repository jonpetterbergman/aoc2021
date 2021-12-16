(defun get-lines (stream)
  (loop for x = (read-line stream nil nil)
     while x
     collect x))

(defun get-rule (line)
  (cons (cons (char line 0) (char line 1))
	(char line 6)))

(defun make-rules-table (rules)
  (loop for (k . v) in rules
     with res = (make-hash-table :test 'equal)
     do (setf (gethash (cons (car k) (cdr k)) res) v)
     finally (return res)))

(defun get-data (stream)
  (let ((lines (get-lines stream)))
    (values (coerce (car lines) 'list)
	    (make-rules-table (mapcar #'get-rule (cddr lines))))))

(defun get-pairs (xs)
  (loop for subl on xs
     while (cdr subl)
     collect (cons (car subl) (cadr subl))))

(defun count-item (alist item)
  (let ((old (assoc item alist)))
    (if old
	(acons item (1+ (cdr old)) (delete old alist))
	(acons item 1 alist))))

(defun merge-occ (alist1 alist2)
  (let ((keys (remove-duplicates
	       (append (mapcar #'car alist1)
		       (mapcar #'car alist2))
	       :test 'equal)))
    (mapcar
     (lambda (key)
       (cons key
	     (let ((e1 (assoc key alist1))
		   (e2 (assoc key alist2)))
	       (+ (if e1 (cdr e1) 0)
		  (if e2 (cdr e2) 0)))))
     keys)))

(defun decrement-occ (alist el)
  (mapcar
   (lambda (item)
     (if (equal el (car item))
	 (cons (car item) (1- (cdr item)))
	 item))
   alist))

(defun simple-occ (pattern)
  (loop for x in pattern
     with res = nil
     do (setf res (count-item res x))
     finally (return res)))

(defun apply-rule (pair rules)
  (list (car pair) (gethash pair rules) (cdr pair)))

(defun get-occ (pattern cache rules n)
  (or (gethash (cons pattern n) cache)
      (if (= n 0)
	  (simple-occ pattern)
	  (let ((r (loop for pair on (get-pairs pattern)
		      with res = nil
		      do (setf res (merge-occ (get-occ (apply-rule (car pair) rules) cache rules (1- n)) res))
		      if (cdr pair) do (setf res (decrement-occ res (cdr (car pair))))
		      finally (return res))))
	    (setf (gethash (cons pattern n) cache) r)
	    r))))

(defun test (n)
  (with-open-file (stream "aoc14.testdata")
    (multiple-value-bind (pattern rules)
	(get-data stream)
      (get-occ pattern (make-hash-table :test 'equal) rules n))))

(defun run (n)
  (with-open-file (stream "aoc14.input")
    (multiple-value-bind (pattern rules)
	(get-data stream)
      (let ((r (mapcar #'cdr (get-occ pattern (make-hash-table :test 'equal) rules n))))
	(- (reduce #'max r) (reduce #'min r))))))
