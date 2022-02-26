(in-package #:aoc2021)

(defun 15-read-input (path)
  (iter (for line in-file path using #'read-line)
    (collect (map 'list #'digit-char-p line))))


(defun 15-make-board-1 (lines)
  (make-array (list (length lines) (length (first lines)))
              :initial-contents lines))


(defun 15-delete-min (list &key (key #'identity) (test #'equalp))
  (let ((min-el (iter (for el in list)
                  (finding el minimizing (funcall key el)))))
    (list (delete min-el list :test test) min-el)))


(defmacro 15-handle-neighbour (x2 y2)
  `(let ((new-dist (+ (aref cave ,x2 ,y2) (aref dists x y))))
     (when (= most-positive-fixnum #1=(aref dists ,x2 ,y2))
       (push (cons ,x2 ,y2) queue))
     (when (< new-dist #1#)
       (setf #1# new-dist)
       (setf (aref preds ,x2 ,y2) (cons x y)))))


(defun 15-backtrack-path (x y cave preds)
  (iter (for (cx . cy) first (cons x y) then (aref preds cx cy))
    (until (and (zerop cx)
                (zerop cy)))
    (sum (aref cave cx cy))))


;; single-source-shortest-path / Dijkstra's Algorithm
;; However the performance with an ad hoc queue is good enough,
;; no need for a fibonacci oder binary heap.
(defun 15-solve-1 (&optional (path "15/15.txt"))
  (let* ((cave (15-make-board-1 (15-read-input path)))
         (dists (make-array (array-dimensions cave) :initial-element most-positive-fixnum))
         (preds (make-array (array-dimensions cave) :initial-element nil))
         (queue (list (cons 0 0)))
         (rows (array-dimension cave 0))
         (cols (array-dimension cave 1)))
    (setf (aref dists 0 0) 0)
    (iter (for (q (x . y)) = (15-delete-min queue :key (lambda (a)
                                                      (aref dists (car a) (cdr a)))))
      (setf queue q)
      (unless (zerop x)
        (15-handle-neighbour (1- x) y))
      (unless (zerop y)
        (15-handle-neighbour x (1- y)))
      (unless (= x (1- rows))
        (15-handle-neighbour (1+ x) y))
      (unless (= y (1- cols))
        (15-handle-neighbour x (1+ y)))
      (when (and (= x (1- rows))
                 (= y (1- cols)))
        (leave (15-backtrack-path x y cave preds))))))


(defun 15-multiply-line (line)
  (append line
          (mapcar #'1+ line)
          (mapcar (curry #'+ 2) line)
          (mapcar (curry #'+ 3) line)
          (mapcar (curry #'+ 4) line)))

(defun 15-make-board-2 (short-lines)
  (let* ((lines (mapcar #'15-multiply-line short-lines))
         (all-lines (append lines
                            (mapcar (lambda (l) (mapcar #'1+ l)) lines)
                            (mapcar (lambda (l) (mapcar (curry #'+ 2) l)) lines)
                            (mapcar (lambda (l) (mapcar (curry #'+ 3) l)) lines)
                            (mapcar (lambda (l) (mapcar (curry #'+ 4) l)) lines)))
         (cave (make-array (list (length all-lines)
                                 (length (first all-lines)))
                           :initial-contents all-lines)))
    (iter (for i below (array-total-size cave))
      (setf #1=(row-major-aref cave i) (if (> #1# 9) (- #1# 9) #1#)))
    cave))


(defun 15-solve-2 (&optional (path "15/15.txt"))
  (let* ((cave (15-make-board-2 (15-read-input path)))
         (dists (make-array (array-dimensions cave) :initial-element most-positive-fixnum))
         (preds (make-array (array-dimensions cave) :initial-element nil))
         (queue (list (cons 0 0)))
         (rows (array-dimension cave 0))
         (cols (array-dimension cave 1)))
    (setf (aref dists 0 0) 0)
    (iter (for (q (x . y)) = (15-delete-min queue :key (lambda (a)
                                                      (aref dists (car a) (cdr a)))))
      (setf queue q)
      (unless (zerop x)
        (15-handle-neighbour (1- x) y))
      (unless (zerop y)
        (15-handle-neighbour x (1- y)))
      (unless (= x (1- rows))
        (15-handle-neighbour (1+ x) y))
      (unless (= y (1- cols))
        (15-handle-neighbour x (1+ y)))
      (when (and (= x (1- rows))
                 (= y (1- cols)))
        (leave (15-backtrack-path x y cave preds))))))
