(defun bigp (cave)
  (upper-case-p (char cave 0)))

(defun decode-path (line)
  (loop for subl on line
     for chr = (car subl)
     for rest = (cdr subl)
     if (equal chr #\-) return (cons (coerce val 'string) (coerce rest 'string))
     collect chr into val))

(defun read-paths (stream)
  (loop for x = (read-line stream nil nil)
     while x
     collect (decode-path (coerce x 'list))))

(defun caves (paths)
  (remove-duplicates
   (append
    (mapcar #'car paths)
    (mapcar #'cdr paths))
   :test #'equal))

(defun connected-to (paths cave)
  (loop for (start . end) in paths
     if (equal cave start) collect end into result
     if (equal cave end) collect start into result
     finally (return result)))

(defun find-paths (paths start small)
  (remove-duplicates
   (mapcar
    (lambda (subp)
      (cons start subp))
    (mapcan
     (lambda (next)
       (if (equal next "end")
	   (list (list "end"))
	   (if (bigp next)
	       (find-paths paths next small)
	       (if (not (member next small :test #'equal))
		   (find-paths paths next (adjoin next small :test #'equal))
		   nil))))
     (connected-to paths start)))
   :test #'equal))

(defun has-double (xs)
  (loop for x on xs
     if (member (car x) (cdr x) :test #'equal) return (car x)
     finally (return nil)))

(defun find-paths-allow-one-repeat (paths start small)
  (remove-duplicates
   (mapcar
    (lambda (subp)
      (cons start subp))
    (mapcan
     (lambda (next)
       (if (equal next "end")
	   (list (list "end"))
	   (if (equal next "start")
	       nil
	       (if (bigp next)
		   (find-paths-allow-one-repeat paths next small)
		   (if (not (and (has-double small)
				 (member next small :test #'equal)))
		       (find-paths-allow-one-repeat paths next (cons next small))
		       nil)))))
     (connected-to paths start)))
   :test #'equal))


(defun test ()
  (with-open-file (stream "aoc12.testdata")
    (let ((res (find-paths (read-paths stream) "start" '("start"))))
      (values res (length res)))))

(defun test-small ()
  (with-open-file (stream "aoc12.smalldata")
    (let ((res (find-paths (read-paths stream) "start" '("start"))))
      (values res (length res)))))

(defun test-tiny ()
  (with-open-file (stream "aoc12.tiny")
    (let ((res (find-paths (read-paths stream) "start" '("start"))))
      (values res (length res)))))

(defun run ()
  (with-open-file (stream "aoc12.input")
    (let ((res (find-paths (read-paths stream) "start" '("start"))))
      (values res (length res)))))

(defun test-tiny-2 ()
  (with-open-file (stream "aoc12.tiny")
    (let ((res (find-paths-allow-one-repeat (read-paths stream) "start" '("start"))))
      (values res (length res)))))

(defun test-small-2 ()
  (with-open-file (stream "aoc12.smalldata")
    (let ((res (find-paths-allow-one-repeat (read-paths stream) "start" '("start"))))
      (values res (length res)))))

(defun test-2 ()
  (with-open-file (stream "aoc12.testdata")
    (let ((res (find-paths-allow-one-repeat (read-paths stream) "start" '("start"))))
      (values res (length res)))))

(defun run-2 ()
  (with-open-file (stream "aoc12.input")
    (let ((res (find-paths-allow-one-repeat (read-paths stream) "start" '("start"))))
      (length res))))

