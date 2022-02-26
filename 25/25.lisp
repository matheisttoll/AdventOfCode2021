(in-package #:aoc2021)

(defun 25-read-input (path)
  (let ((result (iter (for line in-file path using #'read-line)
                  (collect line))))
    (make-array (list (length result) (length (first result)))
                :initial-contents result)))


(defmacro 25-sea-floor (floor x y)
  `(aref ,floor (mod ,x (array-dimension ,floor 0))
                (mod ,y (array-dimension ,floor 1))))


(defun 25-empty? (sea-floor x y)
  (char= #\. (25-sea-floor sea-floor x y)))


(defun 25-east-moving (floor x y)
  (char= #\> (aref floor x y)))


(defun 25-south-moving (floor x y)
  (char= #\v (aref floor x y)))


(defun 25-step (floor &aux something-moved)
  (destructuring-bind (n m) (array-dimensions floor)
    (let ((can-move (make-array (list n m) :initial-element nil)))
      (iter (for x below n)
        (iter (for y below m)
          (when (and (25-east-moving floor x y)
                     (25-empty? floor x (1+ y)))
            (setf (aref can-move x y) t)
            (setf something-moved t))))
      (iter (for x below n)
        (iter (for y below m)
          (when (aref can-move x y)
            (rotatef (25-sea-floor floor x y) (25-sea-floor floor x (1+ y))))))
      (setf can-move (make-array (list n m) :initial-element nil))
      (iter (for x below n)
        (iter (for y below m)
          (when (and (25-south-moving floor x y)
                     (25-empty? floor (1+ x) y))
            (setf (aref can-move x y) t)
            (setf something-moved t))))
      (iter (for x below n)
        (iter (for y below m)
          (when (aref can-move x y)
            (rotatef (25-sea-floor floor x y) (25-sea-floor floor (1+ x) y)))))
    something-moved)))


(defun 25-print-floor (floor)
  (iter (for x below (array-dimension floor 0))
    (iter (for y below (array-dimension floor 1))
      (princ (aref floor x y)))
    (terpri))
  (terpri))


(defun 25-solve-1 (&optional (path "25/25.txt"))
  (let ((floor (25-read-input path)))
    (iter (for i from 1)
          (while (25-step floor))
          (finally (return i)))))
