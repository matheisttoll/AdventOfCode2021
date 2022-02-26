(in-package #:aoc2021)

(defun 08-sort-string (string)
  (sort string #'char<=))


(defun 08-read-input (path)
  (iter (for line in-file path using #'read-line)
        (for entries = (mapcar (rcurry #'sort #'char<=)
                               (split "( \\| )| " line)))
    (collect (list (subseq entries 0 10) (subseq entries 10)))))


(defun 08-solve-1 (&optional (path "08/08.txt"))
  (let ((input (08-read-input path))
        (result 0))
    (iter (for (_ out-num) in input)
      (declare (ignorable _))
      (iter (for out in out-num)
        (when (find (length out) '(2 3 4 7))
          (incf result))))
    result))


(defun 08-compute-all-combinations (&optional (example (list "cagedb"
                                                             "ab"
                                                             "gcdfa"
                                                             "fbcad"
                                                             "eafb"
                                                             "cdfbe"
                                                             "cdfgeb"
                                                             "dab"
                                                             "acedgfb"
                                                             "cefabd")))

  (let ((all-combinations ()))
    (map-permutations
     (lambda (perm)
       (push (mapcar (lambda (word)
                       (08-sort-string (map 'string
                                            (lambda (c)
                                              (char perm
                                                    (- (char-int c) 97)))
                                            word)))
                     example)
             all-combinations))
     "abcdefg")
    all-combinations))


(defun 08-solve-2 (&optional (path "08/08.txt"))
  (let ((all-combinations (08-compute-all-combinations)))
    (iter (for (all nums) in (08-read-input path))
          (for relations = (find-if-not (rcurry #'set-exclusive-or
                                                all :test #'equalp)
                                        all-combinations))
      (sum (parse-integer (map 'string (lambda (n)
                                         (digit-char (position n relations
                                                               :test #'equalp)))
                               nums))))))
