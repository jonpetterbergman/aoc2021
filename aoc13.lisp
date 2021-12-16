(defun decode-point (str)
  (loop for (chr . rest) on (coerce str 'list)
     while (not (equal chr #\,))
     collect chr into xstr
     finally (return (cons (parse-integer (coerce xstr 'string))
			   (parse-integer (coerce rest 'string))))))

(defun read-lines (stream)
  (loop for l = (read-line stream nil nil)
     while l
     collect l))

(defun decode-fold (line)
  (let ((axis (read-from-string (subseq line 11 12)))
	(val  (parse-integer (subseq line 13 (length line)))))
    (cons axis val)))

(defun read-data (stream)
  (loop for (l . rest) on (read-lines stream)
     while (not (equal l ""))
     collect (decode-point l) into result
     finally (return (cons result (mapcar #'decode-fold rest)))))

(defun fold-along-x (point x)
  (let ((px (car point))
	(py (cdr point)))
    (cons (if (> px x) (- x (- px x)) px)
	  py)))

(defun fold-along-y (point y)
  (let ((px (car point))
	(py (cdr point)))
    (cons px
	  (if (> py y) (- y (- py y)) py))))

(defun make-fold (point foldinst)
  (let ((dir (car foldinst))
	(val (cdr foldinst)))
    (if (equal dir 'X)
	(fold-along-x point val)
	(fold-along-y point val))))

(defun fold-points (points foldinst)
  (loop for point in points
     collect (make-fold point foldinst) into result
     finally (return (remove-duplicates result :test #'equal))))

(defun fold-all (points foldinsts)
  (loop for foldinst in foldinsts
     do (setf points (fold-points points foldinst))
     finally (return points)))
       

(defun test ()
  (with-open-file (stream "aoc13.testdata")
    (let* ((inp (read-data stream))
	   (points (car inp))
	   (foldinsts (cdr inp)))
      (fold-points points (car foldinsts)))))

(defun run ()
  (with-open-file (stream "aoc13.input")
    (let* ((inp (read-data stream))
	   (points (car inp))
	   (foldinsts (cdr inp)))
      (length (fold-points points (car foldinsts))))))
