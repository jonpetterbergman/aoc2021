(defun get-next-bit-vector (stream)
  (let ((line (read-line stream nil nil)))
    (and line (with-input-from-string (s (concatenate 'string "#*" line)) (read s)))))

(defun bit-vector-to-integer (bits)
  (loop for bit across bits
     with start = 0
     do (setf start (+ (* start 2) bit))
     finally (return start)))

(defun get-gamma-epsilon (filename)
  (with-open-file (stream filename)
    (loop for bits = (get-next-bit-vector stream)
       while bits
       with ones  = (make-array 12 :element-type 'integer :initial-element 0)
       do (loop for value across bits
	     for x from 0
	     do (setf (aref ones x) (+ (aref ones x) (if (= 1 value) 1 -1))))
       finally (return (loop for os across ones
			  for ix from 0
			  with obits = (make-array 12 :element-type 'bit)
			  do (setf (aref obits ix) (if (> os 0) 1 0))
			  finally (return (* (bit-vector-to-integer obits)
					     (bit-vector-to-integer (bit-not obits)))))))))

(defun get-list (filename)
  (with-open-file (stream filename)
    (loop for bits = (get-next-bit-vector stream)
       while bits
       collect bits)))

(defun get-most-common-bit (blist)
  (loop for bits in blist
     with ones = (make-array 12 :element-type 'integer :initial-element 0)
     do (loop for value across bits
	   for x from 0
	   do (setf (aref ones x) (+ (aref ones x) (if (= 1 value) 1 -1))))
     finally (return (loop for os across ones
			for ix from 0
			with obits = (make-array 12 :element-type 'bit)
			do (setf (aref obits ix) (if (>= os 0) 1 0))
			finally (return obits)))))

(defun select-number (blist filter-bits invert)
  (loop for bit across filter-bits
     for x from 0
     do (loop for bits in blist
	   if (= (aref bits x) (aref filter-bits x)) collect bits into selection
	   finally (setf blist selection))
     do (setf filter-bits (if invert
			      (bit-not (get-most-common-bit blist))
			    (get-most-common-bit blist)))
     if (= (length blist) 1) return (car blist)
     finally (return (car blist))))

(defun get-life-support (filename)
  (let* ((blist (get-list filename))
	 (filter-bits (get-most-common-bit blist)))
    (* (bit-vector-to-integer (select-number blist filter-bits nil))
       (bit-vector-to-integer (select-number blist (bit-not filter-bits) t)))))
