(defun get-data (stream)
  (loop for line = (read-line stream nil nil)
     while line
     collect line))

(defun to-arr (lines)
  (let ((w (length (car lines)))
	(h (length lines)))
    (loop for y from 0 to (1- h)
       for line in lines
       with arr = (make-array (list h w))
       do (loop for x from 0 to (1- w)
	     do (setf (aref arr y x)
		      (parse-integer (subseq line x (1+ x)))))
       finally (return arr))))

(defun copy-array (arr)
  (let ((w (array-dimension arr 1))
	(h (array-dimension arr 0)))
    (loop for y from 0 to (1- h)
       with narr = (make-array (list h w))
       do (loop for x from 0 to (1- w)
	     do (setf (aref narr y x)
		      (aref arr y x)))
       finally (return narr))))

(defun larger-array (arr)
  (let ((w (array-dimension arr 1))
	(h (array-dimension arr 0)))
    (loop for y from 0 to (1- h)
       with narr = (make-array (list (* h 5) (* w 5)))
       do (loop for x from 0 to (1- w)
	     do (loop for yfact from 0 to 4
		   do (loop for xfact from 0 to 4
			   do (setf (aref narr (+ (* yfact h) y) (+ (* xfact w) x))
				    (1+ (mod (+ (+ yfact xfact) (1- (aref arr y x))) 9))))))
       finally (return narr))))
  

(defun right (arr x y)
  (if (< x (1- (array-dimension arr 1)))
      (aref arr y (1+ x))
      nil))

(defun down (arr x y)
  (if (< y (1- (array-dimension arr 0)))
      (aref arr (1+ y) x)
      nil))

(defun up (arr x y)
  (if (> y 0)
      (aref arr (1- y) x)
      nil))

(defun left (arr x y)
  (if (> x 0)
      (aref arr y (1- x))
      nil))

(defun score (oarray arr x y)
  (declare (ignore oarray))
  (let* ((d      (down arr x y))
	 (r      (right arr x y))
	 (this   (if (and (= x 0) (= y 0)) 0 (aref arr y x)))
	 (mlist (delete-if #'not (list d r))))
    (+ this (if mlist
		(reduce #'min mlist)
		0))))

(defun score-2 (oarray arr x y)
  (let* ((d      (down arr x y))
	 (r      (right arr x y))
	 (u      (up arr x y))
	 (l      (left arr x y))
	 (this   (if (and (= x 0) (= y 0)) 0 (aref oarray y x)))
	 (prev   (aref arr y x))
	 (mlist (delete-if #'not (list d r u l))))
    (if (> prev this)
	(+ this (if mlist
		    (reduce #'min mlist)
		    0))
	prev)))

(defun solve (oarray arr scorer)
  (let ((w (array-dimension arr 1))
	(h (array-dimension arr 0)))
    (loop for x from (1- w) downto 0
       do (loop for y from (1- h) downto 0
	     do (setf (aref arr y x) (apply scorer (list oarray arr x y))))
       finally (return arr))
    (aref arr 0 0)))

(defun test ()
  (with-open-file (stream "aoc15.testdata")
    (let ((arr (to-arr (get-data stream))))
      (solve arr arr #'score))))

(defun run ()
  (with-open-file (stream "aoc15.input")
    (let* ((arr  (to-arr (get-data stream)))
	   (narr (copy-array arr)))
      (solve narr arr #'score)
      (loop for x = (solve narr arr #'score-2)
	 for n from 0 to 20
	 do (print x)))))

(defun test2 ()
  (with-open-file (stream "aoc15.testdata")
    (let* ((arr  (to-arr (get-data stream)))
	   (larr (larger-array arr))
	   (narr (copy-array larr)))
      (solve narr larr #'score)
      (print narr)
      (loop for x = (solve narr larr #'score-2)
	 for n from 0 to 50
	 do (print x)))))

(defun run2 ()
  (with-open-file (stream "aoc15.input")
    (let* ((arr  (to-arr (get-data stream)))
	   (larr (larger-array arr))
	   (narr (copy-array larr)))
      (solve narr larr #'score)
      (loop for x = (solve narr larr #'score-2)
	 for n from 0 to 50
	 do (print x)))))

