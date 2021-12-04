(defun get-random (stream)
  (loop for rn = (read stream)
     collect rn
     while (equal (read-char stream) #\,)))

(defun get-board (stream)
  (loop for row from 0 to 4
     with arr = (make-array '(5 5))
     do (loop for col from 0 to 4
	   do (setf (aref arr row col) (read stream)))
     finally (return arr)))

(defun get-boards (stream)
  (loop while (peek-char t stream nil nil)
     collect (get-board stream)))

(defun get-data (filename)
  (with-open-file (stream filename)
    (values (get-random stream)
	    (get-boards stream))))

(defun mark (num boards)
  (loop for board in boards
     do (loop for row from 0 to 4
	   do (loop for col from 0 to 4
		 do (when (= (or (aref board row col) -1) num)
		      (setf (aref board row col) nil))))
     finally (return boards)))

(defun sum-board (board)
  (loop for row from 0 to (- (array-dimension board 0) 1)
     sum (loop for col from 0 to (- (array-dimension board 1) 1)
	    sum (or (aref board row col) 0))))

(defun eval-board (board)
  (or (loop for row from 0 to (- (array-dimension board 0) 1)
	 if (every #'not (loop for col from 0 to (- (array-dimension board 1) 1)
			    collect (aref board row col))) return (sum-board board) end)
      (loop for col from 0 to (- (array-dimension board 1) 1)
	 if (every #'not (loop for row from 0 to (- (array-dimension board 0) 1)
			    collect (aref board row col))) return (sum-board board) end)))

(defun eval-boards (boards)
  (loop for board in boards
     if (eval-board board) return it))

(defun remove-winning-board (boards)
  (remove-if #'eval-board boards)) 

(defun run (nums boards)
  (loop for num in nums
     do (mark num boards)
     if (eval-boards boards) return (* num (eval-boards boards))))

(defun run2 (nums boards)
  (loop for (num . rest) on nums
     do (mark num boards)
     do (setf boards (remove-winning-board boards))
     while (> (length boards) 1)
     finally (return (run rest boards))))
