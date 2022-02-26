(in-package #:aoc2021)

(defparameter *10-points* (list (cons nil 0)
                                (cons #\) 3)
                                (cons #\] 57)
                                (cons #\} 1197)
                                (cons #\> 25137)))

(defun 10-read-input (path)
  (iter (for line in-file path using #'read-line)
    (collect line)))


(defun 10-first-illegal-char (string &aux (stack ()) last)
  (iter (for c in-vector string)
        (finally (return (values nil stack)))
    (if (find c "([{<")
        (push c stack)
        (unless (find-if (curry #'string= (coerce (vector (setf last (pop stack)) c) 'string))
                         '("()" "[]" "{}" "<>"))
          (leave (values c nil))))))


(defun 10-solve-1 (&optional (path "10/10.txt"))
  (reduce #'+ (10-read-input path) :key (lambda (line) (cdr (assoc (10-first-illegal-char line) *10-points*)))))


(defparameter *10-costs* (list (cons #\( 1)
                               (cons #\[ 2)
                               (cons #\{ 3)
                               (cons #\< 4)))


(defun 10-compute-completation-cost (stack &aux (result 0))
  (iter (for c in stack)
    (setf result (* 5 result))
    (incf result (cdr (assoc c *10-costs*))))
  result)


(defun 10-solve-2 (&optional (path "10/10.txt"))
  (let ((stacks (remove nil (mapcar (lambda (line)
                                      (nth-value 1 (10-first-illegal-char line)))
                                    (10-read-input path)))))
    (let ((costs (sort (mapcar #'10-compute-completation-cost stacks) #'<)))
      (nth (floor (length costs) 2) costs))))
