(defun string-to-producer (str)
  (format t str)
  (terpri)
  (let ((ix 0))
    (lambda ()
      (if (< ix (length str))
	  (let ((tchr (char str ix)))
	    (prog1
		tchr
	      (format t (string tchr))
	      (finish-output *standard-output*)
	      (setf ix (1+ ix))))
	  nil))))

(defun get-rule (line)
  (cons (cons (char line 0) (char line 1))
	(char line 6)))

(defun get-lines (stream)
  (loop for x = (read-line stream nil nil)
     while x
     collect x))

(defun pair-to-symbol (a b)
  (+ (* (char-code a) 256) (char-code b)))

(defun make-rules-table (rules)
  (loop for (k . v) in rules
     with res = (make-array (* 256 256))
     do (setf (aref res (pair-to-symbol (car k) (cdr k))) v)
     finally (return res)))

(defun get-data (stream)
  (let ((lines (get-lines stream)))
    (values (car lines)
	    (make-rules-table (mapcar #'get-rule (cddr lines))))))

(defun apply-rules (pair rules)
  (loop for (pattern . insert) in rules
     if (equal pattern pair) return (list (car pair) insert (cdr pair))
     finally (return pair)))

(defun pairs (xs)
  (loop for x on xs
     while (> (length x) 1)
     collect (cons (car x) (cadr x))))

(defun join2 (l1 l2)
 (if l1
     (append l1 (cdr l2))
     l2))

(defun step-prod (pprod rules)
  (declare (type function pprod))
  (let ((queue (list (apply pprod '()))))
    (lambda ()
      (if (cdr queue)
	  (pop queue)
 	  (let ((next (apply pprod '())))
	    (if (not next)
		(pop queue)
		(let ((ins (aref rules (pair-to-symbol (car queue) next))))
		  (if (not ins)
		      (progn
			(setf queue (append queue (list next)))
			(pop queue))
		      (progn
			(setf queue (append queue (list ins next)))
			(pop queue))))))))))


(defun step-times (pattern rules n)
  (loop for x from 1 to n
     with final = (string-to-producer pattern)
     do (setf final (step-prod final rules))
     finally (return final)))
  
(defun run-step-3 (pattern rules)
  (loop for x from 0 to (- (length pattern) 2)
     with res = (make-string (* 2 (length pattern)))
     with resix = 0
     do (let* ((a   (char pattern x))
	       (b   (char pattern (1+ x)))
	       (ins (gethash (cons a b) rules)))
	  (if ins
	      (progn (setf (char res resix) a)
		     (setf (char res (+ resix 1)) ins)
		     (setf (char res (+ resix 2)) b)
		     (setf resix (+ resix 2)))
	      (progn (setf (char res resix) a)
		     (setf (char res (+ resix 1)) b)
		     (setf resix (+ resix 1)))))
     finally (return (subseq res 0 (1+ resix)))))

(defun run-step-2 (pattern rules)
  (loop with subl = pattern
     while (cddr subl)
     do (let ((ins (gethash (cons (car subl) (cadr subl)) rules)))
	  (if ins
	      (progn (setf (cdr subl) (cons ins (cdr subl)))
		     (setf subl (cddr subl)))
	      (setf subl (cdr subl))))
     finally (return pattern)))

(defun run-step (pattern rules)
  (reverse (loop for pair in (pairs pattern)
	      with res = nil
	      do (let ((ires (reverse (apply-rules pair rules))))
		   (setf res (if res
				 (append ires (cdr res))
				 ires)))
	      finally (return res))))

(defun repeat-step (pattern rules n)
  (loop for x from 1 to n
     with res = pattern
     do (gc :full t) 
     do (print x)
     do (setf res (run-step-3 res rules))
     finally (return res)))

(defun hist (list)
  (loop for x = (apply list '())
     while x
     with table = (make-array 256)
     do (setf (aref table (char-code x))
	      (let
		  ((old (aref table (char-code x))))
		(if old
		    (1+ old)
		    1)))
     finally (return table)))

(defun hash-to-alist (h)
  (loop for x being each hash-key of h
     for v = (gethash x h)
     if (/= v 0) collect (cons x v) into result
     finally (return result)))

(defun find-max (al)
  (reduce
   (lambda (x b)
     (if (> (cdr x) (cdr b))
	 x
	 b))
   al))

(defun find-min (al)
  (reduce
   (lambda (x b)
     (if (< (cdr x) (cdr b))
	 x
	 b))
   al))


(defun init-arr (pattern n)
  (let ((arr (make-array n :initial-element nil)))
    (setf (aref arr 0) (coerce pattern 'list))
    arr))

(defun first-pair (arr)
  (loop for x from (1- (length arr)) downto 0
     if (cdr (aref arr x)) return x
     finally (return 0)))

(defun down (arr rules)
  (loop for x from (first-pair arr) to (- (length arr) 2)
     for val = (aref arr x)
     for ins = (if (cdr val) (aref rules (pair-to-symbol (car val) (cadr val))) nil)
     do (setf (aref arr (1+ x))
	      (join2 (aref arr (1+ x)) (if (cdr val) (list (car val) ins (cadr val)) val)))
     do (pop (aref arr x))
     finally (return arr)))

(defun down-shave (arr rules)
  (loop while (aref arr 0)
     with result = (make-hash-table :test 'equal)
     do (down arr rules)
     do (loop for x in (cdr (aref arr (1- (length arr))))
	   do (setf (gethash x result) (1+ (or (gethash x result) 0))))       
     do (setf (aref arr (1- (length arr))) nil)
     finally (progn
	       (setf (gethash #\N result) (1+ (gethash #\N result))) 
	       (return result))))
       
(defun test (n)
  (with-open-file (stream "aoc14.testdata")
    (multiple-value-bind (pattern rules)
	(get-data stream)
      (let ((al (hash-to-alist (down-shave (init-arr pattern n) rules))))
	(- (cdr (find-max al))
	   (cdr (find-min al)))))))

(defun run ()
  (with-open-file (stream "aoc14.input")
    (multiple-value-bind (pattern rules)
	(get-data stream)
      (let ((al (hash-to-alist (down-shave (init-arr pattern 11) rules))))
	(- (cdr (find-max al))
	   (cdr (find-min al)))))))

(defun run2 (n)
  (with-open-file (stream "aoc14.input")
    (multiple-value-bind (pattern rules)
	(get-data stream)
      (let ((al (hash-to-alist (down-shave (init-arr pattern n) rules))))
	(- (cdr (find-max al))
	   (cdr (find-min al)))))))
