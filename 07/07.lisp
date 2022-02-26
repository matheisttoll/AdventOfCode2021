(in-package #:aoc2021)

(defun 07-read-input (path)
  (mapcar #'parse-integer (split "," (read-file-into-string path))))


(defun 07-cost-of-position (pos input)
  (iter (for other in input)
        (sum (abs (- pos other)))))


(defun 07-cost-of-position-2 (pos input)
  (iter (for other in input)
        (for d = (abs (- pos other)))
        (sum (* d (1+ d) 1/2))))


(defun 07-solve-1 (&optional (path "07/07.txt") &aux (input (07-read-input path)))
  (iter (for pos in input)
        (finally (return (iter (for p from min-pos to max-pos)
                               (minimizing (07-cost-of-position p input)))))
        (minimize pos into min-pos)
        (maximize pos into max-pos)))


(defun 07-solve-2 (&optional (path "07/07.txt") &aux (input (07-read-input path)))
  (iter (for pos in input)
        (finally (return (iter (for p from min-pos to max-pos)
                               (minimizing (07-cost-of-position-2 p input)))))
        (minimize pos into min-pos)
        (maximize pos into max-pos)))
