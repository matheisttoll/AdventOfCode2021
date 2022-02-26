(in-package #:aoc2021)


(defun 06-read-input (&optional (path "06.txt"))
  (let ((fish-pop (make-array 9 :initial-element 0)))
    (mapcar (lambda (s)
              (incf (aref fish-pop (parse-integer s))))
            (split-string (read-file-into-string path) :separator ","))
    fish-pop))


(defun 06-population-change (fish-pop)
  (let ((new-pop (make-array 9 :initial-element 0)))
    (incf (aref new-pop 6) (aref fish-pop 0))
    (incf (aref new-pop 8) (aref fish-pop 0))
    (iter (for i from 1 below (length fish-pop))
          (incf (aref new-pop (1- i)) (aref fish-pop i)))
    new-pop))


(defun 06-solve (days path)
  (iter (repeat days)
        (for fish-pop initially (06-read-input path) then (06-population-change fish-pop))
        (finally (return (reduce #'+ fish-pop)))))


(defun 06-solve-1 (&optional (path "06/06.txt"))
  (06-solve 80 path))


(defun 06-solve-2 (&optional (path "06/06.txt"))
  (06-solve 256 path))

