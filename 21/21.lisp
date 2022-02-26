(in-package #:aoc2021)

(defun 21-read-input (path)
  (iter (for line in-file path using #'read-line)
        (for player =
             (nth-value 1 (scan-to-strings "Player \\d starting position: (\\d)"
                                           line)))
    (collect (1- (parse-integer (elt player 0))))))


(defun 21-solve-1 (&optional (path "21/21.txt"))
  (destructuring-bind (p1 p2) (21-read-input path)
    (let ((s1 0) (s2 0) (d 0))
      (iter (for i from 0)
            (while (and (< s1 1000) (< s2 1000)))
            (finally (return (* (min s1 s2) d)))
        (if (evenp i)
            (incf s1 (1+ (setf p1 (mod (+ p1 (incf d) (incf d) (incf d)) 10))))
            (incf s2 (1+ (setf p2 (mod (+ p2 (incf d) (incf d) (incf d)) 10)))))))))


(defun 21-count-occurencies (list &key (test 'eql))
  (let ((occurs (make-hash-table :test test)))
    (iter (for e in list)
      (incf (gethash e occurs 0)))
    (sort (hash-table-alist occurs) #'> :key #'cdr)))


(defparameter *21-dice-occurencies*
  (let ((result nil))
    (iter (for i from 1 to 3)
      (iter (for j from 1 to 3)
        (iter (for k from 1 to 3)
          (push (+ i j k) result))))
    (21-count-occurencies result)))


(defstruct (21-state
            (:constructor 21-make-state (player1 player2
                                         &optional (score1 0) (score2 0) (turn 0))))
  player1
  player2
  score1
  score2
  turn)


(defun 21-state-won (state)
  (cond ((>= (21-state-score1 state) 21) 0)
        ((>= (21-state-score2 state) 21) 1)))


(defun 21-move-pawn (state steps)
  (if (evenp (21-state-turn state))
      (let ((new-p1 (mod (+ steps (21-state-player1 state)) 10)))
        (21-make-state new-p1 (21-state-player2 state)
                       (+ new-p1 1 (21-state-score1 state))
                       (21-state-score2 state)
                       (1+ (21-state-turn state))))
      (let ((new-p2 (mod (+ steps (21-state-player2 state)) 10)))
        (21-make-state (21-state-player1 state) new-p2
                       (21-state-score1 state)
                       (+ new-p2 1 (21-state-score2 state))
                       (1+ (21-state-turn state))))))


(defun 21-move-pawns (state weight states points)
  (iter (for (m . w) in *21-dice-occurencies*)
        (for new-state = (21-move-pawn state m))
        (for new-weight = (* weight w))
    (if-let ((winner (21-state-won new-state)))
      (incf (aref points winner) new-weight)
      (incf (gethash (21-move-pawn state m) states 0) new-weight)))
  states)


(defun 21-step (states points &aux (new-states (make-hash-table :test 'equalp)))
  (iter (for (state weight) in-hashtable states)
    (21-move-pawns state weight new-states points))
  new-states)


;; first possibility for victory: after the third move of player 1
;; last possible move: the sixth move of player 1
(defun 21-solve-2 (&optional (path "21/21.txt"))
  (destructuring-bind (p1 p2) (21-read-input path)
    (let ((states (make-hash-table :test 'equalp))
          (points (vector 0 0)))
      (setf (gethash (21-make-state p1 p2) states) 1)
      (iter (for cur-states initially states then (21-step cur-states points))
            (until (zerop (hash-table-count cur-states))))
      (max (aref points 0) (aref points 1)))))
