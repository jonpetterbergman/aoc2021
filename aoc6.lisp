(defun get-start-pop (stream)
  (cons (read stream)
	(loop while (read-char stream nil nil)
	   collect (read stream))))

(defun next-gen (pop)
  (loop for lfish in pop
     with ngen = nil
     if (= lfish 0) do (setf ngen (append '(6 8) ngen))
     else do (setf ngen (cons (- lfish 1) ngen))
     finally (return ngen)))


(defun get-result (pop repetitions)
  (loop for x from 0 to (- repetitions 1)
     do (setf pop (next-gen pop))
     finally (return (length pop))))

(defun test ()
  (with-open-file (stream "aoc6.testdata")
    (get-result (get-start-pop stream) 80)))

(defun test2 ()
  (with-open-file (stream "aoc6.testdata")
    (mget-result (pop-to-mpop (get-start-pop stream)) 256)))

(defun run ()
  (with-open-file (stream "aoc6.input")
    (get-result (get-start-pop stream) 80)))

(defun run2 ()
  (with-open-file (stream "aoc6.input")
    (mget-result (pop-to-mpop (get-start-pop stream)) 256)))


(defun pop-to-mpop (pop)
  (loop for lfish in pop
     with acc = (make-array 9 :element-type 'integer :initial-element 0)
     do (setf (aref acc lfish) (+ 1 (aref acc lfish)))
     finally (return acc)))

(defun mnext-gen (mpop)
  (vector
   (aref mpop 1)
   (aref mpop 2)
   (aref mpop 3)
   (aref mpop 4)
   (aref mpop 5)
   (aref mpop 6)
   (+ (aref mpop 7) (aref mpop 0))
   (aref mpop 8)
   (aref mpop 0)))

(defun mget-result (mpop repetitions)
  (loop for x from 1 to repetitions
     do (setf mpop (mnext-gen mpop))
     finally (return (loop for pos across mpop sum pos))))
