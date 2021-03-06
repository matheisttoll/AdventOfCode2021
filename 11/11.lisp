(in-package #:aoc2021)

(defun 11-read-input (path)
  (let ((lines (iter (for line in-file path using #'read-line)
                 (collect (map 'vector #'digit-char-p line)))))
    (make-array (list (length lines) (length (first lines)))
                :initial-contents lines)))


(defun 11-step (array flashed rows cols &aux (flashes 0))
  (iter (for r from 0 below rows)
    (iter (for c from 0 below cols)
      (when (and (> (aref array r c) 9)
                 (not #1=(aref flashed r c)))
        (incf flashes)
        (setf #1# t)
        (unless (= r 0)
          (incf (aref array (1- r) c)))
        (unless (= r (1- rows))
          (incf (aref array (1+ r) c)))
        (unless (= c 0)
          (incf (aref array r (1- c))))
        (unless (= c (1- cols))
          (incf (aref array r (1+ c))))
        (unless (or (= r 0) (= c 0))
          (incf (aref array (1- r) (1- c))))
        (unless (or (= r 0) (= c (1- cols)))
          (incf (aref array (1- r) (1+ c))))
        (unless (or (= r (1- rows)) (= c 0))
          (incf (aref array (1+ r) (1- c))))
        (unless (or (= r (1- rows)) (= c (1- cols)))
          (incf (aref array (1+ r) (1+ c)))))))
  flashes)


(defun 11-time-step (array)
  (destructuring-bind (rows cols) (array-dimensions array)
    (let ((flashed (make-array (list rows cols) :initial-element nil))
          (result 0))
      (iter (for r from 0 below rows)
        (iter (for c from 0 below cols)
          (incf (aref array r c))))
      (iter (for flashes = (11-step array flashed rows cols))
            (until (zerop flashes))
        (incf result flashes))
      (iter (for r from 0 below rows)
        (iter (for c from 0 below cols)
          (when (aref flashed r c)
            (setf (aref array r c) 0))))
      result)))


(defun 11-solve-1 (&optional (path "11/11.txt"))
  (let ((array (11-read-input path)))
    (iter (repeat 100)
      (sum (11-time-step array)))))


(defun 11-solve-2 (&optional (path "11/11.txt"))
  (let ((array (11-read-input path)))
    (iter (for i from 1)
          (for flashes = (11-time-step array))
          (when (= flashes (array-total-size array))
             (leave i)))))
