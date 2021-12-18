(defconstant tinydata1 "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")

(defun parse-num (stream)
  (parse-integer
   (coerce
    (loop while (digit-char-p (peek-char t stream nil nil))
       collect (read-char stream nil nil))
    'string)))

(defun parse-pair (stream)
  (read-char stream nil nil)
  (let ((first (parse stream)))
    (read-char stream nil nil)
    (prog1
      (cons first (parse stream))
      (read-char stream nil nil))))

(defun parse (stream)
  (let ((next (peek-char t stream nil nil)))
    (and next
	 (if (char= next #\[)
	     (parse-pair stream)
	     (parse-num stream)))))

(defun get-data (filename)
  (with-open-file (stream filename)
    (loop for x = (read-line stream nil nil)
       while x
       collect (with-input-from-string (s x) (parse s)))))

(defun explode (item)
  (if (and (numberp (car item)) (numberp (cdr item)))
      (values t (car item) (cdr item) 0)
      (error "explode applied to illegal pair")))

(defun split (n)
  (values t 0 0 (cons (floor (/ n 2))
		      (ceiling (/ n 2)))))

(defun add-to-leftmost (item n)
  (if (numberp item)
      (+ item n)
      (cons (add-to-leftmost (car item) n)
	    (cdr item))))

(defun add-to-rightmost (item n)
  (if (numberp item)
      (+ item n)
      (cons (car item)
	    (add-to-rightmost (cdr item) n))))

(defun reduce-split (item level)
  (cond
    ((numberp item)
     (if (>= item 10)
	 (split item)
	 (values nil 0 0 item)))
    (t (multiple-value-bind (arestart aleft aright aresult)
	   (reduce-split (car item) (1+ level))
	 (multiple-value-bind (brestart bleft bright bresult)
	     (reduce-split (cdr item) (1+ level))
	   (cond
	     (arestart
	      (values arestart aleft 0 (cons aresult (add-to-leftmost (cdr item) aright))))
	     (brestart
		(values brestart 0 bright (cons (add-to-rightmost (car item) bleft) bresult)))
	     (t
	      (values nil 0 0 item))))))))

(defun reduce-explode (item level)
  (cond
    ((numberp item) (values nil 0 0 item))
    ((>= level 4)
     (explode item))
    (t (multiple-value-bind (arestart aleft aright aresult)
	   (reduce-explode (car item) (1+ level))
	 (multiple-value-bind (brestart bleft bright bresult)
	     (reduce-explode (cdr item) (1+ level))
	   (cond
	     (arestart
	      (values arestart aleft 0 (cons aresult (add-to-leftmost (cdr item) aright))))
	     (brestart
	      (values brestart 0 bright (cons (add-to-rightmost (car item) bleft) bresult)))
	     (t
	      (values nil 0 0 item))))))))

(defun show-nice (item)
  (if (numberp item)
      (write-to-string item)
      (concatenate 'string "[" (show-nice (car item)) ","
		   (show-nice (cdr item)) "]")))

(defun reduce-once (item)
  (multiple-value-bind (prestart pright pleft pnext)
      (reduce-explode item 0)
    (if (equal item pnext)
	(multiple-value-bind (restart right left next)
	    (reduce-split pnext 0)
	  next)
	pnext)))

(defun reduce-all (prev)
  (let ((next (reduce-once prev)))
    (if (equal next prev)
	prev
	(reduce-all next))))

(defun reduce-list (xs)
  (loop for x in xs
     with res = nil
     if res do (setf res (reduce-all (cons res x)))
     else do (setf res x)
     finally (return res)))

(defun magnitude (item)
  (if (numberp item)
      item
      (+ (* 3 (magnitude (car item)))
	 (* 2 (magnitude (cdr item))))))


(defun max-mag (a b)
  (max (magnitude (reduce-all (cons a b)))
       (magnitude (reduce-all (cons b a)))))

(defun find-max-mag (xs)
  (loop for x on xs
     while (cdr x)
     maximize (loop for y in (cdr x)
		 maximize (max-mag (car x) y))))

(defun test ()
  (reduce-list (get-data "aoc18.testdata1")))

(defun test-small1 ()
  (reduce-list (get-data "aoc18.small1")))

(defun test-small2 ()
  (reduce-list (get-data "aoc18.small2")))

(defun test-small3 ()
  (reduce-list (get-data "aoc18.small3")))

(defun test-tiny ()
  (with-input-from-string (stream tinydata1)
    (reduce-all (parse stream))))

(defun test-homework ()
  (magnitude (reduce-list (get-data "aoc18.homework"))))

(defun run ()
  (magnitude (reduce-list (get-data "aoc18.input"))))

(defun test2 ()
  (find-max-mag (get-data "aoc18.homework")))

(defun run2 ()
  (find-max-mag (get-data "aoc18.input")))
