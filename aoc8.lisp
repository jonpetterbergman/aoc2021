(defun get-entry (stream)
  (let ((signatures (loop for digit = (read stream nil nil)
		       while digit
		       collect (coerce (string digit) 'list)
		       while (not (equal (peek-char nil stream) #\|)))))
    (and signatures
	 (read-char stream)
	 (cons signatures (loop for digit = (read-preserving-whitespace stream)
			       collect (coerce (string digit) 'list)
			       while (let ((pc (peek-char nil stream nil nil)))
				       (and pc (not (equal pc #\Newline)))))))))


(defun get-val (sequence)
  (loop for digit in sequence
     for len = (length digit)
     if (= len 2) collect 1 
     else if (= len 3) collect 7
     else if (= len 4) collect 4
     else if (= len 7) collect 8))

(defun count-digits (stream)
  (loop for entry = (get-entry stream)
     while entry
     for signatures = (car entry)
     for sequence = (cdr entry)
     sum (length (get-val sequence))))

(defun decode-1-7-4-8 (sigs)
  (mapcar (lambda (sig)
	    (case (length sig)
	      ((2) (cons 1 sig))
	      ((3) (cons 7 sig))
	      ((4) (cons 4 sig))
	      ((7) (cons 8 sig))
	      (otherwise nil)))
	  sigs))


(defun decode-6-3 (dict sigs)
  (mapcar (lambda (sig)
	    (case (length sig)
	      ((5) (let ((one (cdr (assoc 1 dict))))
		     (if (subsetp one sig)
			 (cons 3 sig)
			 nil)))
	      ((6) (let ((one (cdr (assoc 1 dict))))
		     (if (subsetp one sig)
			 nil
			 (cons 6 sig))))
	      (otherwise (rassoc sig dict))))
	  sigs))

(defun decode-9-0 (dict sigs)
  (mapcar (lambda (sig)
	    (case (length sig)
	      ((6) (let ((three (cdr (assoc 3 dict))))
		     (if (subsetp three sig)
			 (cons 9 sig)
			 (or (rassoc sig dict) (cons 0 sig)))))
	      (otherwise (rassoc sig dict))))
	  sigs))

(defun decode-5-2 (dict sigs)
  (mapcar (lambda (sig)
	    (or (rassoc sig dict)
		(let* ((nine (cdr (assoc 9 dict)))
		       (three (cdr (assoc 3 dict)))
		       (uleft (set-difference nine three)))
		  (if (subsetp uleft sig)
		      (cons 5 sig)
		      (cons 2 sig)))))
	  sigs))
		   
(defun srassoc (key xs)
  (loop for (v . k) in xs
     if (and (= (length k) (length key))
	     (loop for kel in key
		if (not (member kel k)) return nil
		finally (return t)))
      return v
     finally (return nil)))

(defun count-all (stream)
  (let ((entries (loop for entry = (get-entry stream)
		    while entry
		    collect entry)))
    (loop for entry in entries
       for sequence = (cdr entry)
       for signatures = (car entry)
       for dict = (decode-5-2
		   (decode-9-0
		    (decode-6-3
		     (decode-1-7-4-8
		      signatures)
		     signatures)
		    signatures)
		   signatures)
       sum (parse-integer
	    (format nil "~{~a~}"
		    (loop for digit in sequence
		       collect (write-to-string (srassoc digit dict))))))))


(defun test ()
  (with-open-file (stream "aoc8.testdata")
    (count-digits stream)))

(defun test2 ()
  (with-open-file (stream "aoc8.testdata")
    (count-all stream)))


(defun run ()
  (with-open-file (stream "aoc8.input")
    (count-digits stream)))

(defun run2 ()
  (with-open-file (stream "aoc8.input")
    (count-all stream)))
