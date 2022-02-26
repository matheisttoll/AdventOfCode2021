(in-package #:aoc2021)


(defun 17-read-input (path &aux (str (read-file-into-string path)))
  (map 'list #'parse-integer
       (nth-value 1 (scan-to-strings
                     "target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)"
                     str))))


;; Travelrange in x direction
(defun 17-x-range (velo &optional (steps velo))
  (- (* (1+ velo) velo 1/2)
     (* (- velo steps) (- velo steps 1) 1/2)))


;; First number of steps that reaches over x,
;; assumes such a number of steps is possible!
(defun 17-find-step (velo x)
  (let* ((phalf (/ (1+ (* 2 velo)) 2))
         (tmp (- (expt phalf 2) (* 2 x)))
         (candidate (floor (- phalf (sqrt tmp)))))
    (if (= (17-x-range velo candidate) x)
        candidate
        (1+ candidate))))


;; steps after x value is in target area
;; little Gauß: (n+1)n / 2
(defun 17-x-in-target (x-velo x1 x2)
  (let ((range (17-x-range x-velo)))
    (cond ((< range x1) nil) ;; Cannot reach targe area
          ;; velocity reaches 0 in target area
          ((<= range x2) (list (17-find-step x-velo x1)))
          ;; passes through targe area
          (t (let ((min-steps (17-find-step x-velo x1))
                   (max-steps (17-find-step x-velo x2)))
               (when (= (17-x-range x-velo max-steps) x2)
                 (incf max-steps))
               (if (>= min-steps max-steps)
                   nil
                   (list min-steps (1- max-steps))))))))


;; For the y position we assume that the target is below the submarine.
;; Since the story implies it and it is true for my input it should be fine.
;; The possible situations are a bit different for y since it will not stop
;; completely

;; x-steps does not seem to matter (at leat for my input) since the probe can
;; be shot in way that it remains in the x-target area.
;; For the y-target observe the following:
;; The probe will always reach level 0 again after being shot up, however
;; in the next step it will move down one more than the speed it was shot up.
;; Therefore the highest y-velocity at launch is simply:
;; (1- (abs [lower y-bound of target array]))
(defun 17-solve-1 (&optional (path "17/17.txt"))
  (destructuring-bind (x1 x2 y1 y2) (17-read-input path)
    (declare (ignore y2))
    (let ((x-steps (iter (for x-velo from 1 to x2)
                         (for steps = (17-x-in-target x-velo x1 x2))
                     (when steps
                       (collect (cons x-velo steps))))))
      (iter (for xs in x-steps)
        (format t "~a~%" xs))
      (17-x-range (1- (abs y1))))))


(defun 17-solve-2 (&optional (path "17/17.txt"))
  (declare (ignore path))
  (warn "Second part of day 17 is not solved yet!~%"))
