(in-package #:aoc2021)

(defun 02-line-reader (&optional (stream *standard-input*) (eof-error-p t)
                         eof-value recursive-p)
  (multiple-value-bind (line mn-p) (read-line stream eof-error-p eof-value recursive-p)
    (values (if mn-p
                line
                (destructuring-bind (keyw val) (split-string line)
                  (list keyw (parse-integer val))))
            mn-p)))



(defun 02-solve-1 (&optional (path "02/02.txt") &aux (pos (cons 0 0)))
  (iter (for (cmd val) in-file path using #'02-line-reader)
    (eswitch (cmd :test #'string=)
      ("forward" (incf (car pos) val))
      ("down" (incf (cdr pos) val))
      ("up" (decf (cdr pos) val))))
  (values (* (car pos) (cdr pos))
          pos))


(defun 02-solve-2 (&optional (path "02/02.txt") &aux (pos (cons 0 0)) (aim 0))
  (iter (for (cmd val) in-file path using #'02-line-reader)
    (eswitch (cmd :test #'string=)
      ("forward" (incf (car pos) val) (incf (cdr pos) (* aim val)))
      ("down" (incf aim val))
      ("up" (decf aim val))))
  (values (* (car pos) (cdr pos))
          pos))

