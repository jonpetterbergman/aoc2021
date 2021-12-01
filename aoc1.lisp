(defun count-increase (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
	  with last = nil
          sum (let ((value (parse-integer line)))
		(prog1
		    (if last
			(if (> value last) 1 0)
		      0)
		  (setf last value))))))

(defun m3-cons (x m3)
  (cons x (loop for el in m3
		for i from 1 to 2
		collect el)))
  
(defun m3-sum (m3)
  (if (< (length m3) 3)
      nil
    (apply #'+ m3)))

(defun count-sliding-increase (filename)
  (with-open-file (stream filename)
		  (loop for line = (read-line stream nil)
			while line
			with last = nil
			sum (let* ((value (parse-integer line))
				   (nsum  (m3-cons value last)))
			      (print nsum)
			      (prog1
				  (if (and (= (length last) 3) (= (length nsum) 3))
				      (if (> (m3-sum nsum) (m3-sum last)) 1 0)
				    0)
				(setf last nsum))))))
					  
