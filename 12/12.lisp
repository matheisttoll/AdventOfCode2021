(in-package #:aoc2021)

(defun 12-large-cave-p (cave)
  (upper-case-p (char cave 0)))


;; represent the graph as hashtable of adjacency lists
(defun 12-read-input (path)
  (let ((caves (make-hash-table :test 'equalp)))
    (iter (for line in-file path using #'read-line)
          (for (v w) = (split "-" line))
      (push w (gethash v caves))
      (push v (gethash w caves)))
    caves))


(defun 12-solve-1 (&optional (path "12/12.txt"))
  (let ((caves (12-read-input path))
        (queue (list (list "start")))
        (result 0))
    (iter (until (endp queue))
      (for path = (pop queue))
      (iter (for w in (gethash (first path) caves))
        (cond ((string= w "end")
               (incf result))
              ((or (12-large-cave-p w)
                   (not (find w path :test #'string=)))
               (push (cons w path) queue)))))
    result))


(defstruct (12-path (:constructor 12-start-path ())
                    (:constructor 12-make-path (path &optional double-cave)))
  (double-cave nil)
  (path (list "start")))


(defun 12-append-path (path cave end-reached-fun)
  (cond ((string= cave "end")
         (funcall end-reached-fun (12-make-path (cons "end" (12-path-path path))
                                                (12-path-double-cave path)))
         (values nil))
        ((12-large-cave-p cave)
         (12-make-path (cons cave (12-path-path path))
                       (12-path-double-cave path)))
        ((not (find cave (12-path-path path) :test #'string=))
         (12-make-path (cons cave (12-path-path path))
                       (12-path-double-cave path)))
        ((and (not (string= cave "start"))
              (not (12-path-double-cave path)))
         (12-make-path (cons cave (12-path-path path))
                       cave))))


(defun 12-solve-2 (&optional (file-path "12/12.txt"))
  (let ((caves (12-read-input file-path))
        (queue (list (12-start-path)))
        (result 0))
    (iter (until (endp queue))
      (for path = (pop queue))
      (iter (for w in (gethash (first (12-path-path path)) caves))
            (for p = (12-append-path path w (lambda (path)
                                              (declare (ignore path))
                                              (incf result))))
        (when p
          (push p queue))))
    result))
