(in-package #:aoc2021)

(defun 20-read-input (path)
  (let (algo
        (image (make-hash-table :test 'equalp)))
    (iter (for line in-file path using #'read-line)
          (for i from -2)
      (when (first-iteration-p)
        (setf algo (map 'vector (lambda (c) (ecase c (#\. 0) (#\# 1))) line)))
      (when (>= i 0)
        (iter (for c in-vector line with-index j)
          (when (char= c #\#)
            (setf (gethash (cons i j) image) 1)))))
    (values image algo)))


(defun 20-lookup-new-pixel (pixel image algo default)
  (let ((binary (with-output-to-string (str)
                  (iter (for i from -1 to 1)
                    (iter (for j from -1 to 1)
                      (princ (gethash (cons (+ (car pixel) i)
                                            (+ (cdr pixel) j))
                                      image
                                      default)
                             str))))))
    (aref algo (parse-integer binary :radix 2))))


;; only pixel with at least one "interesting" neighbour need to
;; considered seperately
(defun 20-step (image algo default)
  (let ((new-image (make-hash-table :test 'equalp)))
    (iter (for (x . y) in (hash-table-keys image))
      (iter (for i from -1 to 1)
        (iter (for j from -1 to 1)
              (for pixel = (cons (+ x i) (+ y j)))
              (for new-val = (20-lookup-new-pixel pixel image algo (- 1 default)))
          (unless (= default new-val)
            (setf (gethash pixel new-image) new-val)))))
    new-image))


;; This code only solves inputs with # as first character
;; i.e. the example will fail
(defun 20-solve (steps path)
  (multiple-value-bind (image algo) (20-read-input path)
    (when (zerop (aref algo 0))
      (error "This case is not covered by this algorithm!"))
    (iter (for i from 1 to steps)
          (for cur-image initially image then (20-step cur-image algo (mod i 2)))
          (finally (return (hash-table-count cur-image))))))


(defun 20-solve-1 (&optional (path "20/20.txt"))
  (20-solve 2 path))

(defun 20-solve-2 (&optional (path "20/20.txt"))
  (20-solve 50 path))
