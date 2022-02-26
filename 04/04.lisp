(in-package #:aoc2021)

(defclass 04-board ()
  ((entries :initarg :entries :accessor entries)
   (marked  :initform (make-array '(5 5) :initial-element nil)
            :accessor marked)))


(defun 04-parse-board-row (string)
  (mapcar #'parse-integer (split " +" (string-trim " " string))))


(defun 04-parse-board (string)
  (let ((entries (mapcar #'04-parse-board-row
                         (split-string (string-trim (list #\Newline) string)
                                       :separator (list #\Newline)))))
    (make-instance '04-board
                   :entries (make-array '(5 5) :initial-contents entries))))


(defun 04-read-input (&optional (path "04.txt"))
  (destructuring-bind (num-string &rest board-strings)
      (split (format nil "~%~%") (read-file-into-string path))
    (let ((numbers (mapcar #'parse-integer (split "," num-string)))
          (boards (mapcar #'04-parse-board board-strings)))
      (list numbers boards))))


(defun 04-check-marked (marked row col)
  (iter (for k from 0 below 5)
    (finally (return-from 04-check-marked t))
    (unless (aref marked row k)
      (leave)))
  (iter (for k from 0 below 5)
    (finally (return-from 04-check-marked t))
    (unless (aref marked k col)
      (leave)))
  (return-from 04-check-marked nil))


(defun 04-mark-number-on-board (board number)
  (with-slots (entries marked) board
    (iter (for i from 0 below 5)
      (iter (for j from 0 below 5)
        (when (= number (aref entries i j))
          (setf (aref marked i j) t)
          (return-from 04-mark-number-on-board (04-check-marked marked i j)))))))


(defun 04-eval-score (board number &aux (unmarked-sum 0))
  (with-slots (entries marked) board
    (iter (for i from 0 below 5)
      (iter (for j from 0 below 5)
        (unless (aref marked i j)
          (incf unmarked-sum (aref entries i j))))))
  (* number unmarked-sum))


(defun 04-solve-1 (&optional (path "04/04.txt"))
  (destructuring-bind (numbers boards) (04-read-input path)
    (iter (for num in numbers)
          (for winner-board = (find-if (lambda (board)
                                         (04-mark-number-on-board board num))
                                       boards))
      (when winner-board
        (leave (04-eval-score winner-board num))))))


(defun 04-solve-2 (&optional (path "04/04.txt"))
  (destructuring-bind (numbers boards) (04-read-input path)
    (iter (for num in numbers)
          (for remaining-boards initially boards
               then (remove-if (lambda (board)
                                 (04-mark-number-on-board board num))
                               remaining-boards))
      (when (and (= 1 (length remaining-boards))
                 (04-mark-number-on-board (first remaining-boards) num))
        (leave (04-eval-score (first remaining-boards) num))))))
