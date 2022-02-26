(in-package #:aoc2021)

(defun 22-read-input (path)
  (let ((regex "(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)"))
    (iter (for line in-file path using #'read-line)
          (for splitted = (nth-value 1 (scan-to-strings regex line)))
      (collect (list (if (string= "on" (aref splitted 0)) 1 0)
                     (cons (parse-integer (aref splitted 1))
                           (parse-integer (aref splitted 2)))
                     (cons (parse-integer (aref splitted 3))
                           (parse-integer (aref splitted 4)))
                     (cons (parse-integer (aref splitted 5))
                           (parse-integer (aref splitted 6))))))))


(defun 22-solve-1 (&optional (path "22/22.txt"))
  (let ((cubes (make-hash-table :test 'equalp)))
    (iter (for (op (x1 . x2) (y1 . y2) (z1 . z2)) in (22-read-input path))
      (unless (or (> x1 50)  (> y1 50)  (> z1 50)
                  (< x2 -50) (< y2 -50) (< z2 -50)))
      (iter (for x from (max x1 -50) to (min x2 50))
        (iter (for y from (max y1 -50) to (min y2 50))
          (iter (for z from (max z1 -50) to (min z2 50))
                (for coords = (vector x y z))
            (if (zerop op)
                (remhash coords cubes)
                (setf (gethash coords cubes) 1))))))
    (hash-table-count cubes)))


(defstruct (22-cuboid (:constructor 22-make-cuboid (x1 x2 y1 y2 z1 z2)))
  x1 x2 y1 y2 z1 z2)


(defun 22-volume (cuboid)
  (let ((cand (* (- (22-cuboid-x2 cuboid)
                    (22-cuboid-x1 cuboid)
                    -1)
                 (- (22-cuboid-y2 cuboid)
                    (22-cuboid-y1 cuboid)
                    -1)
                 (- (22-cuboid-z2 cuboid)
                    (22-cuboid-z1 cuboid)
                    -1))))
    (if (>= cand 0)
        cand
        0)))


(defun 22-intersection (c d)
  (22-make-cuboid (max (22-cuboid-x1 c) (22-cuboid-x1 d))
                  (min (22-cuboid-x2 c) (22-cuboid-x2 d))
                  (max (22-cuboid-y1 c) (22-cuboid-y1 d))
                  (min (22-cuboid-y2 c) (22-cuboid-y2 d))
                  (max (22-cuboid-z1 c) (22-cuboid-z1 d))
                  (min (22-cuboid-z2 c) (22-cuboid-z2 d))))


(defun 22-read-input-2 (path)
  (let ((regex "(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)"))
    (iter (for line in-file path using #'read-line)
          (for splitted = (nth-value 1 (scan-to-strings regex line)))
          (finally (return (coerce instructions 'vector)))
      (collect (cons (apply #'22-make-cuboid
                            (map 'list #'parse-integer (subseq splitted 1)))
                     (if (string= "on" (aref splitted 0)) 1 0))
        into instructions))))


(defun 22-solve-2 (&optional (path "22/22.txt"))
  (declare (ignore path))
  (warn "The second part of day 22 is not implemented."))
