(defun read-instruction (stream)
  (let ((direction (read stream nil nil)))
    (and direction (cons direction (read stream)))))

(defun get-pos (filename)
  (with-open-file (stream filename)
    (loop for instruction = (read-instruction stream)
       with depth = 0
       with pos = 0
       while instruction
       do (let ((ammount (cdr instruction)))
	    (ecase (car instruction)
	      ((FORWARD) (setf pos (+ pos ammount)))
	      ((DOWN)    (setf depth (+ depth ammount)))
	      ((UP)      (setf depth (- depth ammount)))))
       finally (return (* depth pos)))))


(defun get-pos-with-aim (filename)
  (with-open-file (stream filename)
    (loop for instruction = (read-instruction stream)
       with depth = 0
       with pos = 0
       with aim = 0
       while instruction
       do (let ((ammount (cdr instruction)))
	    (ecase (car instruction)
	      ((FORWARD) (progn (setf pos (+ pos ammount))
				(setf depth (+ depth (* ammount aim)))))
	      ((DOWN)    (setf aim (+ aim ammount)))
	      ((UP)      (setf aim (- aim ammount)))))
       finally (return (* depth pos)))))
