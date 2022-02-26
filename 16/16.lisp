(in-package #:aoc2021)

(defstruct (16-message (:conc-name 16-mes-)
                       (:constructor 16-make-message (content &optional (position 0))))
  content
  position)


(defun 16-read-input (path)
  (let* ((string (string-trim (list #\Newline)
                             (read-file-into-string path)))
         (form (format nil "~~~d,'0b" (* 4 (length string)))))
    (16-make-message (format nil form (parse-integer string :radix 16)))))


(defun 16-parse-n-bits-to-integer (message n)
  (parse-integer (16-mes-content message)
                 :start #1=(16-mes-position message)
                 :end (incf #1# n)
                 :radix 2))


(defun 16-parse-version (message)
  (16-parse-n-bits-to-integer message 3))


(defun 16-parse-type-id (message)
  (16-parse-n-bits-to-integer message 3))


(defun 16-parse-length-type-id (message)
  (16-parse-n-bits-to-integer message 1))


(defun 16-parse-subpackage-bits (message)
  (16-parse-n-bits-to-integer message 15))


(defun 16-parse-subpackage-count (message)
  (16-parse-n-bits-to-integer message 11))


(defun 16-parse-literal-content (message &aux (string (16-mes-content message)))
  (parse-integer (with-output-to-string (stream)
                   (iter (for i from #1=(16-mes-position message) by 5)
                     (princ (subseq string (1+ i) (+ i 5)) stream)
                         (while (eql (char string i) #\1))
                         (finally (setf #1# (+ i 5)))))
                 :radix 2))


(defstruct (16-package (:constructor 16-make-package (version type-id &optional content)))
  version
  type-id
  content)


(defvar *16-result-1* 0)


(defun 16-parse-message (message)
  (let* ((version (16-parse-version message))
         (type-id (16-parse-type-id message))
         (package (16-make-package version type-id)))
    (incf *16-result-1* version)
    (setf (16-package-content package)
          (if (= type-id 4)
              (16-parse-literal-content message)
              (if (zerop (16-parse-length-type-id message))
                  (let ((end-of-subpackages (+ (16-parse-subpackage-bits message)
                                               (16-mes-position message))))
                    (iter (while (< (16-mes-position message) end-of-subpackages))
                      (collect (16-parse-message message))))
                  (iter (repeat (16-parse-subpackage-count message))
                    (collect (16-parse-message message))))))
    package))


(defun 16-solve-1 (&optional (path "16/16.txt"))
  (let ((message (16-read-input path))
        (*16-result-1* 0))
    (16-parse-message message)
    *16-result-1*))


(defparameter *16-operations* (vector #'+
                                      #'*
                                      #'min
                                      #'max
                                      (lambda (&rest r)
                                        (declare (ignore r))
                                        (error "No eval for type id 4!"))
                                      (lambda (a b) (if (> a b) 1 0))
                                      (lambda (a b) (if (< a b) 1 0))
                                      (lambda (a b) (if (= a b) 1 0))))


(defun 16-eval-package (package &aux (id (16-package-type-id package)))
  (if (= 4 id)
      (16-package-content package)
      (apply (aref *16-operations* id)
             (mapcar #'16-eval-package (16-package-content package)))))


(defun 16-solve-2 (&optional (path "16/16.txt"))
  (let ((package (16-parse-message (16-read-input path))))
    (16-eval-package package)))
