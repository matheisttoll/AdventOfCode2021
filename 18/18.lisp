(in-package #:aoc2021)

(defun 18-compute-line (line)
  (map 'list (lambda (c) (if-let ((digit (digit-char-p c)))
                              digit
                              c))
        (remove #\, line)))


(defun 18-read-input (path)
  (iter (for line in-file path using #'read-line)
    (collect (18-compute-line line))))


(defun 18-find-if-restlist (predicate list &key (key #'identity))
  (iter (for el on list)
    (when (funcall predicate (funcall key (first el)))
      (leave el))))


(defun 18-explode? (number)
  (iter (for r on number)
        (for c = (car r))
        (with depth = 0)
        (with last-number = nil)
    (cond
      ((and (characterp c) (= 4 depth) (char= c #\[))
       (when last-number
         (incf (car last-number) (second r)))
       (when-let ((rest (18-find-if-restlist #'integerp (cdddr r))))
         (incf (first rest) (third r)))
       (setf (car r) 0
             (cdr r) (cddddr r))
       (leave t))
      ((integerp c) (setf last-number r))
      ((char= #\[ c) (incf depth))
      ((char= #\] c) (decf depth)))))


(defun 18-split? (number)
  (iter (for r on number)
        (for c = (car r))
    (when (and (integerp c) (>= c 10))
      (setf (car r) #\[)
      (let ((insert (append (list (floor (/ c 2)) (ceiling (/ c 2)) #\]) (cdr r))))
        (setf (cdr r) insert))
      (leave t))))


(defun 18-reduce (number)
  (iter (while (or (18-explode? number)
                   (18-split? number))))
  number)


(defun 18-add (a b)
  (18-reduce (append (list #\[) a b (list #\]))))


(defun 18-magnitude (number)
  (let* ((tmp (substitute #\) #\] (substitute #\( #\[ number)))
         (list (read-from-string (with-output-to-string (s)
                                  (dolist (e tmp)
                                    (princ e s)
                                    (princ #\space s))))))
    (labels ((recurse (num)
               (if (integerp num)
                   num
                   (+ (* 3 (recurse (first num))) (* 2 (recurse (second num)))))))
      (recurse list))))


(defun 18-solve-1 (&optional (path "18/18.txt"))
  (18-magnitude (reduce #'18-add (18-read-input path))))


(defun 18-solve-2 (&optional (path "18/18.txt") )
  (let ((input (18-read-input path))
        (result 0))
    (iter (for n in input)
      (iter (for m in input)
        (unless (equalp n m)
          (maxf result (18-magnitude (18-add n m))))))
    result))
