(defun get-positions (stream)
  (cons (read stream)
	(loop while (read-char stream nil nil)
	   collect (read stream))))

(defun get-median (xs)
  (nth (/ (length xs) 2) (sort (copy-list xs) #'<)))

(defun get-avg (xs)
  (floor (/ (loop for x in xs
	       sum x)
	    (length xs))))

(defun test ()
  (with-open-file (stream "aoc7.testdata")
    (let* ((poss (get-positions stream))
	   (med  (get-median poss)))
      (loop for x in poss
	 sum (abs (- x med))))))

(defun run ()
  (with-open-file (stream "aoc7.input")
    (let* ((poss (get-positions stream))
	   (med  (get-median poss)))
      (loop for x in poss
	 sum (abs (- x med))))))

(defun triangular (x)
  (/ (* x (+ x 1)) 2))

(defun test2 ()
  (with-open-file (stream "aoc7.testdata")
    (let* ((poss (get-positions stream))
	   (avg  (get-avg poss)))
      (loop for x in poss
	 sum (triangular (abs (- x avg)))))))

(defun run2 ()
  (with-open-file (stream "aoc7.input")
    (let* ((poss (get-positions stream))
	   (avg  (get-avg poss)))
      (loop for x in poss
	 sum (triangular (abs (- x avg)))))))
