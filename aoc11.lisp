(defun make-levels ()
  (make-array '(10 10) :element-type 'integer))

(defun make-flashes ()
  (make-array '(10 10) :initial-element nil))

(defun get-data (stream)
  (loop for line = (read-line stream nil nil)
     for y from 0 to 9
     while line
     with arr = (make-levels)
     do (loop for chr in (coerce line 'list)
	   for x from 0 to 9
	   do (setf (aref arr y x)
		    (parse-integer (string chr))))
     finally (return arr)))

(defun increment (levels)
  (loop for y from 0 to 9
     do (loop for x from 0 to 9
	   do (setf (aref levels y x)
		    (+ 1 (aref levels y x))))
     finally (return levels)))

(defun neighbors (x y)
  (apply
   #'append
   (loop for ny from (- y 1) to (+ y 1)
      collect (loop for nx from (- x 1) to (+ x 1)
		 if (and (not (and (= nx x)
				   (= ny y)))
			 (>= nx 0)
			 (>= ny 0)
			 (<= nx 9)
			 (<= ny 9))
		 collect (cons nx ny) into result
		 end
		 finally (return result)))))
  
(defun flash (levels flashes x y)
  (when (and (not (aref flashes y x))
	     (> (aref levels y x) 9))
    (loop for (nx . ny) in (neighbors x y)
       do (setf (aref levels ny nx)
		(+ 1 (aref levels ny nx))))
    (setf (aref flashes y x) t)))

(defun flash-pass (levels flashes)
  (loop for y from 0 to 9
     do (loop for x from 0 to 9
	   do (flash levels flashes x y))))

(defun count-flashes (flashes)
  (loop for y from 0 to 9
     sum (loop for x from 0 to 9
	   count (aref flashes y x))))

(defun flash-rec (levels flashes)
  (let ((old-count (count-flashes flashes)))
    (flash-pass levels flashes)
    (if (= old-count (count-flashes flashes))
	old-count
	(flash-rec levels flashes))))

(defun reset-flashed (levels flashes)
  (loop for y from 0 to 9
     do (loop for x from 0 to 9
	   if (aref flashes y x) do (setf (aref levels y x) 0))))

(defun flash-step (levels)
  (increment levels)
  (let ((flashes (make-flashes)))
    (prog1
	(flash-rec levels flashes)
      (reset-flashed levels flashes))))

(defun flash-step-n (levels n)
  (loop for x from 1 to n
     sum (flash-step levels)))

(defun flash-step-n-find-sync (levels n)
  (loop for x from 1 to n
     if (= (flash-step levels) 100) return x))

(defun test ()
  (with-open-file (stream "aoc11.testdata")
    (let* ((start (get-data stream))
	   (count (flash-step-n start 100)))
      (values start count))))

(defun run ()
  (with-open-file (stream "aoc11.input")
    (let* ((start (get-data stream))
	   (count (flash-step-n start 100)))
      (values start count))))

(defun test2 ()
  (with-open-file (stream "aoc11.testdata")
    (let* ((start (get-data stream))
	   (count (flash-step-n-find-sync start 200)))
      (values start count))))


(defun run2 ()
  (with-open-file (stream "aoc11.input")
    (let* ((start (get-data stream))
	   (count (flash-step-n-find-sync start 1000)))
      (values start count))))
