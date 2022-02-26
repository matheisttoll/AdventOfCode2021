(in-package #:aoc2021)

(defun 03-read-input (&optional (path "03.txt"))
  (iter (for bits in-file path using #'read-line)
    (collect (map 'vector #'digit-char-p bits))))


(defun 03-round (x divisor)
  (if (>= (* x 2) divisor)
      1
      0))


(defun 03-most-common-bits (input)
  (let ((result (make-array (length (first input)) :initial-element 0))
        (n (length input)))
  (iter (for bits in input)
        (finally (return (map 'vector (lambda (a) (03-round a n)) result)))
    (iter (for b in-vector bits with-index i)
      (incf (aref result i) b)))))


(defun 03-least-common-bits (input)
  (map 'vector (lambda (b) (- 1 b)) (03-most-common-bits input)))


(defun 03-solve-1 (&optional (path "03/03.txt"))
  (let* ((most-common (03-most-common-bits (03-read-input path)))
         (gamma (map 'string #'digit-char most-common))
         (epsilon (map 'string (lambda (b) (digit-char (- 1 b))) most-common)))
    (* (parse-integer gamma :radix 2)
       (parse-integer epsilon :radix 2))))


(defun 03-filter (list pos filter-fun &aux (filter (funcall filter-fun list)))
  (remove-if-not (lambda (b) (= (aref b pos) (aref filter pos))) list))


(defun 03-solve-2 (&optional (path "03/03.txt") &aux (input (03-read-input path)))
  (let ((oxygen (iter (for i from 0)
                      (for oxy-remainder initially input
                           then (03-filter oxy-remainder i #'03-most-common-bits))
                   (when (= 1 (length oxy-remainder))
                     (leave (first oxy-remainder)))))
         (co2 (iter (for i from 0)
                    (for co2-remainder initially input
                         then (03-filter co2-remainder i #'03-least-common-bits))
                (when (= 1 (length co2-remainder))
                  (leave (first co2-remainder))))))
    (* (parse-integer (map 'string #'digit-char oxygen) :radix 2)
       (parse-integer (map 'string #'digit-char co2) :radix 2))))


