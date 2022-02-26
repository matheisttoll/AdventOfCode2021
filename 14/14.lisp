(in-package #:aoc2021)

(defun 14-string-from-chars (&rest characters)
  (coerce characters 'string))


(defun 14-read-input (path)
  (let ((lines (iter (for line in-file path using #'read-line)
                 (collect line)))
        (rules (make-hash-table :test 'equalp))
        (template (make-hash-table :test 'equalp)))
    (iter (for line in (cddr lines))
          (for (pattern insert) = (split " -> " line))
      (setf (gethash pattern rules)
            (list (14-string-from-chars (char pattern 0) (char insert 0))
                  (14-string-from-chars (char insert 0) (char pattern 1)))))
    (iter (for a in-vector (first lines))
          (for b previous a)
          (finally (return (values template rules (char (first lines) 0) a)))
      (unless (first-iteration-p)
        (incf (gethash (string-from-chars b a) template 0))))))


(defun 14-step (pairs rules)
  (let ((result (make-hash-table :test 'equalp)))
    (iter (for (pattern occurs) in-hashtable pairs)
      (if-let ((new (gethash pattern rules)))
        (iter (for e in new)
          (incf (gethash e result 0) occurs))
        (incf (gethash pattern result 0) occurs)))
    result))


;; Every letter except the first and the last occurs in exactly two patterns
(defun 14-compute-score (polymer start end)
  (let ((result (make-hash-table)))
    (incf (gethash start result 0))
    (incf (gethash end result 0))
    (iter (for (pattern occurs) in-hashtable polymer)
      (incf (gethash (char pattern 0) result 0) occurs)
      (incf (gethash (char pattern 1) result 0) occurs))
    (iter (for (char occurs) in-hashtable result)
          (finally (return (- (/ max-occ 2) (/ min-occ 2))))
      (maximize occurs into max-occ)
      (minimize occurs into min-occ))))


(defun 14-solve (steps &optional (path "14/14.txt"))
  (multiple-value-bind (template rules start end) (14-read-input path)
    (iter (for i below steps)
      (for polymer initially template then (14-step polymer rules))
      (finally (return (14-compute-score polymer start end))))))


(defun 14-solve-1 (&optional (path "14/14.txt"))
  (14-solve 10 path))


(defun 14-solve-2 (&optional (path "14/14.txt"))
  (14-solve 40 path))
