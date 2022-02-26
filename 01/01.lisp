(in-package #:aoc2021)

(defun 01-input (path)
  (iter (for line in-file path using #'read-line)
    (collect (parse-integer line))))

(defun 01-solve-1 (&optional (path "01/01.txt") &aux (input (01-input path)))
  (iter (for depth in input)
        (for prev previous depth)
    (counting (and prev (> depth prev)))))


(defun 01-solve-2 (&optional (path "01/01.txt") &aux (input (01-input path)))
  (iter (for (a b c) on input)
        (unless (and a b c) (finish))
        (for sum = (+ a b c))
        (for prev previous sum)
    (counting (and prev (> sum prev)))))
