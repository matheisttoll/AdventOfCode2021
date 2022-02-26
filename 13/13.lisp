(in-package #:aoc2021)

(defun 13-read-input (path &aux (points-p t))
  (iter (for line in-file path using #'read-line)
        (finally (return (values points folds)))
    (if (string= "" line)
        (setf points-p nil)
        (if points-p
            (destructuring-bind (x y) (mapcar #'parse-integer (split "," line))
              (collect (cons x y) into points))
            (let ((achses (nth-value 1 (scan-to-strings "fold along (\\w)=(\\d+)" line))))
              (collect (cons (char (aref achses 0) 0)
                             (parse-integer (aref achses 1)))
                into folds))))))


(defun 13-perform-fold (points direction val)
  (if (eql direction #\x)
      (iter (for (x . y) in points)
        (collect (cons (if (< x val) x (- val (- x val)))
                       y)))
      (iter (for (x . y) in points)
        (collect (cons x
                       (if (< y val) y (- val (- y val))))))))


(defun 13-solve-1 (&optional (path "13/13.txt"))
  (multiple-value-bind (points folds) (13-read-input path)
    (length (delete-duplicates (13-perform-fold points (caar folds) (cdar folds)) :test #'equalp))))



(defun 13-find-points (points folds)
  (iter (for fold in folds)
    (for cur-points initially points then (13-perform-fold cur-points (car fold) (cdr fold)))
    (finally (return (delete-duplicates cur-points :test #'equalp)))))


(defun 13-solve-2 (&optional (path "13/13.txt"))
  (multiple-value-bind (points folds) (13-read-input path)
    (let* ((remaining (13-find-points points folds))
           (screen (iter (for p in remaining)
                     (finally (return (make-array (list (1+ max-x) (1+ max-y)) :initial-element nil)))
                     (maximize (car p) into max-x)
                     (maximize (cdr p) into max-y))))
      (iter (for p in remaining)
        (setf (aref screen (car p) (cdr p)) t))
      (iter (for j below (array-dimension screen 1))
        (iter (for i below (array-dimension screen 0))
          (if (aref screen i j)
              (princ #\#)
              (princ #\Space)))
        (terpri)))))
