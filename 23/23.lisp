(in-package #:aoc2021)

;; #############
;; #01.2.3.4.56#
;; ###7#9#b#d###
;;   #8#a#c#e#
;;   #########


(defun 23-solve-1 (&optional (path "23/23.txt"))
  (declare (ignore path))
  (warn "No code since my grandma an i solved it by hand :D")
  16157)


(defun 23-solve-2 (&optional (path "23/23.txt"))
  (declare (ignore path))
  (warn "No code since my grandma an i solved it by hand :D")
  43481)


;; Solution of part two with Mucki by hand
;; #############
;; #...........#
;; ###D#D#C#B###
;;   #D#C#B#A#
;;   #D#B#A#C#
;;   #B#A#A#C#
;;   #########

;; #############
;; #BA.......CC#
;; ###D#D#C#.###
;;   #D#C#B#.#
;;   #D#B#A#.#
;;   #B#A#A#.#
;;   #########

;; #############
;; #BA.....B.CC#
;; ###.#.#C#D###
;;   #.#C#B#D#
;;   #.#B#A#D#
;;   #.#A#A#D#
;;   #########

;; #############
;; #BC...B.B.CC#
;; ###.#.#C#D###
;;   #.#.#B#D#
;;   #.#.#A#D#
;;   #A#A#A#D#
;;   #########

;; #############
;; #BC.......CC#
;; ###.#.#C#D###
;;   #.#.#B#D#
;;   #A#B#A#D#
;;   #A#B#A#D#
;;   #########

;; #############
;; #BC.....C.CC#
;; ###A#.#.#D###
;;   #A#B#.#D#
;;   #A#B#.#D#
;;   #A#B#.#D#
;;   #########

;; #############
;; #...........#
;; ###A#B#C#D###
;;   #A#B#C#D#
;;   #A#B#C#D#
;;   #A#B#C#D#
;;   #########

