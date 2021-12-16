(defun read-data-line (stream)
  (loop for x = (read-char stream nil nil)
     while (and x (not (equal x #\Newline)))
     collect (parse-integer (make-string 1 :initial-element x))))

(defun read-data (stream)
  (loop for x = (read-data-line stream)
     while x
     collect x))

(defun data-width (pre-data)
  (length (car pre-data)))

(defun data-height (pre-data)
  (length pre-data))

(defun to-array (pre-data)
  (let* ((w (data-width pre-data))
	 (h (data-height pre-data))
	 (start (make-array (list h w) :element-type 'integer :initial-element 0)))
    (loop for row in pre-data
       for y from 0 to (- h 1)
       do (loop for item in row
	     for x from 0 to (- w 1)
	     do (setf (aref start y x) item))
       finally (return start))))

(defun safe-aref (data y x)
  (and (>= x 0)
       (>= y 0)
       (< x (array-dimension data 1))
       (< y (array-dimension data 0))
       (aref data y x)))

(defun neighbors (data x y)
  (delete nil (list (safe-aref data (- y 1) x)
		    (safe-aref data (+ y 1) x)
		    (safe-aref data y (- x 1))
		    (safe-aref data y (+ x 1)))))

(defun neighbors-pos (x y)
  (list
   (cons (- x 1) y)
   (cons (+ x 1) y)
   (cons x (- y 1))
   (cons x (+ y 1))))

(defun lowestp (data x y)
  (let ((nbors (neighbors data x y))
	(val  (aref data y x)))
    (loop for nbor in nbors
       if (>= val nbor) return nil
       finally (return t))))

(defun risk-level (data)
  (loop for y from 0 to (- (array-dimension data 0) 1)
     sum (loop for x from 0 to (- (array-dimension data 1) 1)
	      sum (if (lowestp data x y) (+ (aref data y x) 1) 0))))

(defun start-basins (data)
  (loop for y from 0 to (- (array-dimension data 0) 1)
     with result = nil
     do (loop for x from 0 to (- (array-dimension data 1) 1)
	   if (lowestp data x y) do (setf result (cons (list (cons x y)) result)))
     finally (return result)))

(defun expand (data basin)
  (union
   basin
   (loop for (x . y) in basin
      for nbors = (set-difference (neighbors-pos x y) basin)
      with result = nil
      do (loop for (nborx . nbory) in nbors
	    for val = (safe-aref data nbory nborx)
	    if (and val (/= val 9)) do (setf result (adjoin (cons nborx nbory) result :test #'equal)))
      finally (return result))
   :test #'equal))

(defun set-equal (set1 set2)
  (and (= (length set1) (length set2))
       (loop for val in set1
	  if (not (member val set2 :test #'equal)) return nil
	  finally (return set1))))

(defun expand-full (data basin)
  (let ((new (expand data basin)))
    (if (set-equal new basin)
	basin
	(expand-full data new))))

(defun expand-all (data basins)
  (loop for basin in basins
     collect (expand-full data basin)))

(defun test ()
  (with-open-file (stream "aoc9.testdata")
    (risk-level (to-array (read-data stream)))))

(defun test2 ()
  (with-open-file (stream "aoc9.testdata")
    (let ((data (to-array (read-data stream))))
      (apply #'* (subseq (sort (mapcar #'length (expand-all data (start-basins data))) #'>) 0 3)))))

(defun run ()
  (with-open-file (stream "aoc9.input")
    (risk-level (to-array (read-data stream)))))

(defun run2 ()
  (with-open-file (stream "aoc9.input")
    (let ((data (to-array (read-data stream))))
      (apply #'* (subseq (sort (mapcar #'length (expand-all data (start-basins data))) #'>) 0 3)))))
