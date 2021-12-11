(defun openp (thechar)
  (case thechar
    ((#\( #\[ #\{ #\<) thechar)
    (otherwise nil)))

(defun close-of (thechar)
  (case thechar
    ((#\() #\))
    ((#\[) #\])
    ((#\{) #\})
    ((#\<) #\>)
    (otherwise nil)))

(defun read-chunk (opener stream)
  (loop for next = (read-char stream nil nil)
     while (openp next)
     if (read-chunk next stream) return it
     finally (if (equal (close-of opener) next)
		 (return nil)
		 (return next))))

			    

(defun points (item)
  (case item
    ((#\)) 3)
    ((#\]) 57)
    ((#\}) 1197)
    ((#\>) 25137)
    (otherwise 0)))

(defun points-2 (item)
  (case item
    ((#\)) 1)
    ((#\]) 2)
    ((#\}) 3)
    ((#\>) 4)
    (otherwise 0)))


(defun read-data (stream)
  (loop for item = (let ((first (read-char stream nil nil)))
		     (and (openp first)
			  (read-chunk first stream)))
     while (peek-char t stream nil nil)
     sum (points item)))


(defun get-score (close-seq)
  (loop for close in close-seq
     with score = 0
     do (setf score (+ (* 5 score) (points-2 close)))
     finally (return score)))

(defun get-close (xs)
  (loop for x in xs
     with stack = nil
     if (openp x) do (setf stack (cons x stack))
     else if (equal x (close-of (car stack))) do (setf stack (cdr stack))
     else return nil
     finally (return (get-score (mapcar #'close-of stack)))))

(defun read-data-lines (stream)
  (loop for l = (read-line stream nil nil)
       while l
       collect (coerce l 'list)))

(defun test ()
  (with-open-file (stream "aoc10.testdata")
    (read-data stream)))

(defun test2 ()
  (with-open-file (stream "aoc10.testdata")
    (let ((seq (sort
		(loop for x in (mapcar #'get-close (read-data-lines stream))
		   if x collect it into result
		   finally (return result))
		#'<)))
      (nth (floor (/ (length seq) 2)) seq))))


(defun run ()
  (with-open-file (stream "aoc10.input")
    (read-data stream)))

(defun run2 ()
  (with-open-file (stream "aoc10.input")
    (let ((seq (sort
		(loop for x in (mapcar #'get-close (read-data-lines stream))
		   if x collect it into result
		   finally (return result))
		#'<)))
      (nth (floor (/ (length seq) 2)) seq))))
