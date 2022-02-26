(in-package #:aoc2021)

(defun 19-read-input (path)
  (let ((scanners (mapcar (curry #'string-trim (format nil " ~t~%"))
                          (split "--- scanner \\d+ ---" (read-file-into-string path)))))
    (iter (for s in (rest scanners))
          (finally (return (coerce scans 'vector)))
      (collect (iter (for beacon in (split-string s :separator (list #\newline)))
                     (finally (return (coerce beacons 'vector)))
                 (collect (map 'list #'parse-integer (split-string beacon
                                                                     :separator ","))
                   into beacons))
        into scans))))


(defun 19-compute-differences (scanners)
  (let ((result (make-array (length scanners))))
    (iter (for scanner in-vector scanners with-index i)
          (for ht = (make-hash-table :test 'equalp))
      (setf (aref result i) ht)
      (iter (for b1 in-vector scanner with-index k)
        (iter (for b2 in-vector scanner from (1+ k) with-index l)
              (for diff = (sort (mapcar (lambda (x y)
                                          (abs (- x y)))
                                        b1 b2)
                                #'<))
          (push k #1=(gethash diff ht ()))
          (push l #1#))))
    result))


(defun 19-difference-equal (diff other)
  (equalp diff other))


(defun 19-shared-beacons (scanners)
  (let ((diffs (19-compute-differences scanners))
        (shared-beacons (make-array (list (length scanners) (length scanners))
                                    :initial-element nil)))
    (iter (for diff in-vector diffs with-index i)
      (iter (for diff2 in-vector diffs with-index j)
        (unless (= i j)
          (iter (for d in (intersection (hash-table-keys diff)
                                        (hash-table-keys diff2)
                                        :test #'19-difference-equal))
                (finally (setf (aref shared-beacons i j)
                               (let ((db (delete-duplicates beacs1 :test #'eql)))
                                 (when (>= (length db) 12)
                                   db))
                               (aref shared-beacons j i)
                               (let ((db (delete-duplicates beacs2 :test #'eql)))
                                 (when (>= (length db) 12)
                                   db))))
            (appending (gethash d diff) into beacs1)
            (appending (gethash d diff2) into beacs2)))))
    shared-beacons))


(defun 19-position-unique? (position sequence)
  (and (not (zerop #1=(aref sequence position)))
       (= 1 (count #1# sequence))))


(defun 19-find-unique-position-in-differences (position points)
  (iter (for p in-vector points with-index k)
    (iter (for q in-vector points from (1+ k) with-index l)
          (for diff = (map 'vector (lambda (a b) (abs (- a b))) p q))
      (when (19-position-unique? position diff)
        (return-from 19-find-unique-position-in-differences
          (cons k l)))))
  (error "My approach is flawed!"))


(defun 19-relative-scanner-position (i j scanners shared-beacons)
  (let* ((si (aref scanners i))
         (sj (aref scanners j))
         (bi (map 'vector (lambda (b) (aref si b)) (aref shared-beacons i j)))
         (bj (map 'vector (lambda (b) (aref sj b)) (aref shared-beacons j i)))
         (bi1 (aref bi 0))
         (bj1 (aref bj 0))
         (orientation (make-array (length bi1)))
         (permutation (make-array (length orientation)))
         (position (make-array (length orientation))))
    (unless (and (>= (length bi) 12)
                 (>= (length bj) 12))
      (return-from 19-relative-scanner-position nil))
    (iter (for k below (length orientation))
          (for (p1 . p2) = (19-find-unique-position-in-differences k bi))
          (for e = (- (elt (aref bi p1) k) (elt (aref bi p2) k)))
          (for diffj = (map 'vector #'- (aref bj p1) (aref bj p2)))
          (for f = (position (abs e) diffj :key #'abs))
          (for s = (signum (aref diffj f)))
      (setf (aref orientation k) (* s (signum e))
            (aref permutation k) f)
      (setf (aref position k)
            (- (elt bi1 k) (* (aref orientation k) (elt bj1 f)))))
    (list orientation permutation position)))


(defun 19-relative-scanner-positions (scanners shared-beacons)
  (let ((n (length scanners))
        (result (make-array (array-dimensions shared-beacons) :initial-element nil)))
    (iter (for i below n)
      (iter (for j  below n)
        (when (aref shared-beacons i j)
          (setf (aref result i j)
                (19-relative-scanner-position i j scanners shared-beacons)))))
    result))


(defun 19-translate-coordinates (coord orientation permutation position)
  (iter (for i below (length coord))
    (collect (+ (aref position i) (* (aref orientation i)
                                     (elt coord (aref permutation i)))))))


(defun 19-find-path-to-scanners (positions)
  (let ((path (make-array (array-dimension positions 0) :initial-element nil))
        (queue (list 0)))
    (iter (until (endp queue))
          (for cur = (pop queue))
      (iter (for i from 1 below (array-dimension positions 0))
        (when (and (/= i cur)
                   (endp (aref path i))
                   (aref positions cur i))
          (setf (aref path i) (cons cur (aref path cur)))
          (push i queue))))
    path))


(defun 19-translate-beacons (scanners positions path)
  (let ((all-beacons (coerce (aref scanners 0) 'list)))
    (iter (for i from 1 below (length scanners))
      (iter (for j in (aref path i))
            (for p previous j initially i)
            (for pos = (aref positions j p))
            (for beacons initially (aref scanners i)
                 then (map 'list (lambda (b)
                                   (apply #'19-translate-coordinates b pos))
                           beacons))
            (finally (appendf all-beacons beacons))))
    (delete-duplicates all-beacons :test #'equalp)))


(defun 19-solve-1 (&optional (path "19/19.txt"))
  (let* ((scanners (19-read-input path))
         (shared-beacons (19-shared-beacons scanners))
         (positions (19-relative-scanner-positions scanners shared-beacons))
         (path (19-find-path-to-scanners positions)))
    (length (19-translate-beacons scanners positions path))))


(defun 19-solve-2 (&optional (path "19/19.txt"))
  (let* ((scanners (19-read-input path))
         (shared-beacons (19-shared-beacons scanners))
         (positions (19-relative-scanner-positions scanners shared-beacons))
         (path (19-find-path-to-scanners positions))
         (scanner-pos (19-translate-beacons (make-array (length scanners)
                                                        :initial-element #((0 0 0)))
                                            positions path))
         (result 0))
    (iter (for pos in scanner-pos)
      (iter (for pos2 in scanner-pos)
        (maxf result (reduce #'+ (mapcar #'- pos pos2) :key #'abs))))
      result))