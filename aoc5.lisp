(defun read-pos (stream)
  (let ((x (read stream nil nil)))
    (and x
	 (progn (read-char stream)
		(cons x (read stream))))))

(defun read-segment (stream)
  (let ((start (read-pos stream)))
    (and start
	 (progn
	   (loop for x from 0 to 2
	      do (read-char stream))
	   (cons start (read-pos stream))))))

(defun read-segments (stream)
  (loop for segment = (read-segment stream)
     while segment
     collect segment))

(defun get-max (selector segments)
  (reduce #'max
   (mapcar (lambda (item)
	     (max (apply selector (list (car item))) (apply selector (list (cdr item)))))
	   segments)))

(defun get-dimensions (segments)
  (values (get-max #'car segments) (get-max #'cdr segments)))

(defun make-board (x y)
  (make-array (list (+ y 1) (+ x 1)) :element-type 'integer :initial-element 0))

(defun paint (board segments)
  (loop for ((x1 . y1) . (x2 . y2)) in segments
     if (= x1 x2) do (loop for y from (min y1 y2) to (max y1 y2)
			do (setf (aref board y x1) (+ (aref board y x1) 1)))
     if (= y1 y2) do (loop for x from (min x1 x2) to (max x1 x2)
			do (setf (aref board y1 x) (+ (aref board y1 x) 1)))
     finally (return board)))

(defun paint-diagonal (board segments)
  (loop for ((x1 . y1) . (x2 . y2)) in segments
     if (= x1 x2) do (loop for y from (min y1 y2) to (max y1 y2)
			do (setf (aref board y x1) (+ (aref board y x1) 1)))
     if (= y1 y2) do (loop for x from (min x1 x2) to (max x1 x2)
			do (setf (aref board y1 x) (+ (aref board y1 x) 1)))
     if (and (/= y1 y2) (/= x1 x2)) do (cond
					 ((and (> x2 x1) (> y2 y1))
					  (loop for x from x1 to x2
					     for y from y1 to y2
					     do (setf (aref board y x) (+ (aref board y x) 1))))
					 ((and (> x2 x1) (< y2 y1))
					  (loop for x from x1 to x2
					     for y from y1 downto y2
					     do (setf (aref board y x) (+ (aref board y x) 1))))
					 ((and (< x2 x1) (> y2 y1))
					  (loop for x from x1 downto x2
					     for y from y1 to y2
					     do (setf (aref board y x) (+ (aref board y x) 1))))
					 ((and (< x2 x1) (< y2 y1))
					  (loop for x from x1 downto x2
					     for y from y1 downto y2
					     do (setf (aref board y x) (+ (aref board y x) 1)))))
     finally (return board)))


(defun find-dangerous (board)
  (loop for row from 0 to (- (array-dimension board 0) 1)
     sum (loop for col from 0 to (- (array-dimension board 1) 1)
	   count (>= (aref board row col) 2))))

(defun test ()
  (with-open-file (stream "aoc5.testdata")
    (let ((segments (read-segments stream)))
      (multiple-value-bind (x y)
	  (get-dimensions segments)
	(find-dangerous (paint (make-board x y) segments))))))

(defun test2 ()
  (with-open-file (stream "aoc5.testdata")
    (let ((segments (read-segments stream)))
      (multiple-value-bind (x y)
	  (get-dimensions segments)
	(find-dangerous (paint-diagonal (make-board x y) segments))))))


(defun run ()
  (with-open-file (stream "aoc5.input")
    (let ((segments (read-segments stream)))
      (multiple-value-bind (x y)
	  (get-dimensions segments)
	(find-dangerous (paint (make-board x y) segments))))))

(defun run2 ()
  (with-open-file (stream "aoc5.input")
    (let ((segments (read-segments stream)))
      (multiple-value-bind (x y)
	  (get-dimensions segments)
	(find-dangerous (paint-diagonal (make-board x y) segments))))))
