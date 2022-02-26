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





;; Use the integers 0, ..., n-1, where n is the number of vertices to
;; represent the vertices in the graph. Save the names of the vertices in
;; an extra array
(defclass undirected-graph ()
  ((vertex-names :accessor vertex-names :initarg :vertex-names :initform nil)
   (vertex-numbers :accessor vertex-numbers :initarg :vertex-numbers)
   (edges :accessor edges :initarg :edges)
   (edge-weights :accessor edge-weights :initarg :edge-weights :initform nil)))


(defun make-undirected-graph (edge-list)
  (let* ((vertices (remove-duplicates (mapcan (lambda (e)
                                               (subseq e 0 2))
                                              edge-list)))
         (v-names (make-array (length vertices) :adjustable t
                                                :fill-pointer t
                                                :initial-contents vertices))
         (v-numbers (make-hash-table))
         (e-weights (make-hash-table :test 'equalp))
         (edges (make-array (length vertices) :initial-element nil)))
    (iter (for name in-vector v-names with-index i)
      (setf (gethash name v-numbers) i))
    (iter (for (v1 v2 weight) in edge-list)
          (for n1 = (gethash v1 v-numbers))
          (for n2 = (gethash v2 v-numbers))
      (setf (gethash (if (< n1 n2) (cons n1 n2) (cons n2 n1)) e-weights) weight)
      (if #1=(aref edges (gethash v1 v-numbers))
          (vector-push-extend #2=(gethash v2 v-numbers) #1#)
          (setf #1# (make-array 1 :initial-element #2# :adjustable t :fill-pointer t)))
      (if #3=(aref edges (gethash v2 v-numbers))
          (vector-push-extend #4=(gethash v1 v-numbers) #3#)
          (setf #3# (make-array 1 :initial-element #4# :adjustable t :fill-pointer t))))
    (make-instance 'undirected-graph
                   :vertex-names v-names
                   :vertex-numbers v-numbers
                   :edges edges
                   :edge-weights e-weights)))



(defmacro aif (condition-form then-form &optional else-form)
  `(let ((it ,condition-form))
     (if it ,then-form ,else-form)))

