(defun extract-bit (num pos)
  (logand (ash num pos) 1))

(defun hex-to-bits (chr)
  (and
   chr
   (let ((num (parse-integer (string chr) :radix 16)))
     (loop for x from -3 upto 0 by 1
	  collect (extract-bit num x)))))

(defun bits-to-num (bit-seq)
  (and bit-seq
       (loop for thebit in (reverse bit-seq)
	  for x from 0
	  with result = 0
	  do (setf result (+ result (* thebit (expt 2 x))))
	  finally (return result))))

(defun make-producer (str)
  (let ((xs    (coerce str 'list))
	(queue nil))
    (lambda ()
      (when (not queue)
	(setf queue (hex-to-bits (pop xs))))
      (pop queue))))

(defun make-stream-producer (stream)
  (let ((queue nil))
    (lambda ()
      (when (not queue)
	(setf queue (hex-to-bits (read-char stream nil nil))))
      (pop queue))))

(defun list-to-producer (xs)
  (lambda () (pop xs)))


(defconstant testdata-1 (make-producer "D2FE28"))

(defconstant testdata-2 (make-producer "38006F45291200"))

(defconstant testdata-3 (make-producer "EE00D40C823060"))

(defun take-n (prod n)
  (loop for x from 1 to n
     for item = (apply prod '())
     while item
     collect item))

(defun get-version (prod)
  (bits-to-num (take-n prod 3)))

(defun get-type (prod)
  (bits-to-num (take-n prod 3)))

(defun parse-literal (prod)
  (loop while (= 1 (apply prod '()))
     append (take-n prod 4) into result
     finally (return (bits-to-num (append result (take-n prod 4))))))

(defun parse-operator (prod)
  (if (= 1 (apply prod '()))
      (loop for n from 1 to (bits-to-num (take-n prod 11))
	 collect (parse-packet prod))
      (loop with p2 = (list-to-producer (take-n prod (bits-to-num (take-n prod 15))))
	 for x = (parse-packet p2)
	 while x
	 collect x)))

(defun op-type (n)
  (case n
    ((0) 'SUM)
    ((1) 'PRODUCT)
    ((2) 'MINIMUM)
    ((3) 'MAXIMUM)
    ((5) 'GREATERTHAN)
    ((6) 'LESSTHAN)
    ((7) 'EQUAL)))

(defun parse-packet (prod)
  (let ((version (get-version prod))
	(ptype (get-type prod)))
    (and version ptype	
	 (case ptype
	   ((4) (list version 'LITERAL (parse-literal prod)))
	   (otherwise (list version (op-type ptype) (parse-operator prod)))))))

(defun collect-versions (tree)
  (let ((ver (car tree))
	(type (cadr tree)))
    (+ ver (if (equal type 'LITERAL)
	       0
	       (loop for item in (caddr tree)
		    sum (collect-versions item))))))

(defun eval-tree (tree)
  (let ((type (cadr tree)))
    (case type
      ((LITERAL)       (caddr tree))
      ((SUM)           (apply #'+ (mapcar #'eval-tree (caddr tree))))
      ((PRODUCT)       (apply #'* (mapcar #'eval-tree (caddr tree))))
      ((MAXIMUM)       (reduce #'max (mapcar #'eval-tree (caddr tree))))
      ((MINIMUM)       (reduce #'min (mapcar #'eval-tree (caddr tree))))
      ((EQUAL)         (if (= (eval-tree (car (caddr tree)))
			      (eval-tree (cadr (caddr tree))))
			   1 0))
      ((LESSTHAN)      (if (< (eval-tree (car (caddr tree)))
			      (eval-tree (cadr (caddr tree))))
			   1 0))
      ((GREATERTHAN)   (if (> (eval-tree (car (caddr tree)))
			      (eval-tree (cadr (caddr tree))))
			   1 0))
      (otherwise       (error (format nil "not implemented: ~a" type))))))

(defun run ()
  (with-open-file (stream "aoc16.input")
    (collect-versions (parse-packet (make-stream-producer stream)))))

(defun run2 ()
  (with-open-file (stream "aoc16.input")
    (eval-tree (parse-packet (make-stream-producer stream)))))
