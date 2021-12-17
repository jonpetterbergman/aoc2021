(defconstant test-area '((20 . 30) . (-10 . -5)))

(defconstant act-area '((156 . 202) . (-110 . -69)))

(defun x-stop (x-speed)
  (loop for this-speed from x-speed downto 0
     with pos = 0
     do (setf pos (+ pos this-speed))
     finally (return pos)))
       
(defun find-x-speeds (target)
  (let ((min-x (caar target))
	(max-x (cdar target)))
    (loop for speed from 1 to 1000
       for stop = (x-stop speed)
       while (<= stop max-x)
       if (>= stop min-x) collect speed)))

(defun hit (target x y)
  (cond
    ((< y (cadr target)) 'under)
    ((and (>= x (caar target))
	  (<= x (cdar target))
	  (>= y (cadr target))
	  (<= y (cddr target)))
     'hit)))

(defun shoot (target x-speed y-speed)
  (loop with x = 0
     with y = 0
     with y-max = 0
     for r = (hit target x y)
;     do (progn (format t "~a, ~a = ~a" x y r) (terpri))
     while (not (equal r 'under))
     if (equal r 'hit) return y-max
     do (setf y-max (max y-max y))
     do (setf x (+ x x-speed))
     do (setf y (+ y y-speed))
     do (setf x-speed (max 0 (- x-speed 1)))
     do (setf y-speed (- y-speed 1))
     finally (return 'under)))

(defun solve (target)
  (loop for x-speed in (find-x-speeds target)
     with y-max = 0
     do (loop for y-speed from 0 to 3000
	   for r = (shoot target x-speed y-speed)
	   ;while (not (equal r 'under))
	   if (numberp r) do (setf y-max (max y-max r)))
     finally (return y-max)))

(defun solve-2 (target)
  (loop for x-speed from 0 to 500
     do (print x-speed)
     with result = nil
     do (loop for y-speed from -300 to 300
	   for r = (shoot target x-speed y-speed)
	   ;while (not (equal r 'under))
	   if (numberp r) do (setf result (adjoin (cons x-speed y-speed) result)))
     finally (return (length result))))
