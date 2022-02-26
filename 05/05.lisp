(in-package #:aoc2021)

(defclass 05-line ()
  ((start :accessor start-point :initarg :start)
   (end   :accessor   end-point :initarg :end)))


(defun 05-parse-line (string)
  (destructuring-bind (a b c d) (mapcar #'parse-integer (split ",| -> " string))
    (make-instance '05-line :start (cons a b) :end (cons c d))))


(defun 05-read-input (&optional (path "05.txt"))
  (iter (for line in-file path using #'read-line)
    (collect (05-parse-line line))))


(defun 05-solve-1 (&optional (path "05/05.txt"))
  (let ((input (05-read-input path))
        (env (make-hash-table :test 'equalp)))
    (iter (for line in input)
      (with-slots (start end) line
        (if (= (car start) (car end))
            (if (> (cdr start) (cdr end))
                (iter (for c from (cdr start) downto (cdr end))
                  (incf (gethash (cons (car start) c) env 0)))
                (iter (for c from (cdr start) to (cdr end))
                  (incf (gethash (cons (car start) c) env 0))))
            (when (= (cdr start) (cdr end))
              (if (> (car start) (car end))
                  (iter (for r from (car start) downto (car end))
                             (incf (gethash (cons r (cdr start)) env 0)))
                  (iter (for r from (car start) to (car end))
                             (incf (gethash (cons r (cdr start)) env 0))))))))
    (iter (for (_ val) in-hashtable env)
      (counting (> val 1)))))


(defun 05-solve-2 (&optional (path "05/05.txt"))
  (let ((input (05-read-input path))
        (env (make-hash-table :test 'equalp)))
    (iter (for line in input)
      (with-slots (start end) line
        (let ((r-step (signum (- (car end) (car start))))
              (c-step (signum (- (cdr end) (cdr start)))))
          (iter (for r from (car start) by r-step)
                (for c from (cdr start) by c-step)
            (incf (gethash (cons r c) env 0))
            (while (and (or (zerop r-step) (/= r (car end)))
                        (or (zerop c-step) (/= c (cdr end)))))))))
    (iter (for (_ val) in-hashtable env)
      (counting (> val 1)))))
