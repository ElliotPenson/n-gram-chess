;;;; n-gram-chess.lisp
;;;; Author: Elliot Penson

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :knight-owl))

(defpackage :n-gram-chess
  (:nicknames :ngc)
  (:use :common-lisp :knight-owl))

(in-package :n-gram-chess)

(defun count-n-grams (list count-map n-gram-size)
  "Record the frequency of n-length groups of elements from a list.
   Add these numbers to the hash-table given as count-map."
  (let ((front-padding (make-list (1- n-gram-size))))
    (loop for successive-list on (append front-padding list)
       while (>= (length successive-list) n-gram-size)
       as n-gram = (subseq successive-list 0 n-gram-size)
       do (incf (gethash n-gram count-map 0))))
  count-map)

(defun train (max-n &optional (corpus-directory "corpus/"))
  "Establish a complete chess move frequency map. Store counts for
   n-grams of all sizes up to (and including) max-n. The
   corpus-directory should contain pgn files."
  (let ((pgn-files (directory (make-pathname :defaults corpus-directory
                                             :name :wild :type "pgn")))
        (count-map (make-hash-table :test #'equalp)))
    (loop for n from 1 to max-n
       do (dolist (pgn-file pgn-files)
            (with-open-file (input pgn-file)
              (loop for (tag-pairs moves) = (pgn-parse input)
                 until (null tag-pairs)
                 do (count-n-grams moves count-map n)))))
    count-map))

(defun move-probability (move previous-moves count-map)
  "Find the likelihood of a chess move given a list of previous moves.
   Calculated as P(move|previous-moves) = P(previous-moves + move) /
   P(previous-moves)."
  (let* ((n-gram (append previous-moves (list move)))
         (n-gram-count (gethash n-gram count-map))
         (previous-count (gethash previous-moves count-map)))
    (if (and n-gram-count previous-count)
        (/ n-gram-count previous-count)
        0)))

(defun valid-moves (board whitep)
  "Find all moves available to the player on the given board. Evaluate
   to a list of moves in algebraic chess notation."
  (remove-if (lambda (move)
               (not (valid-move-p move board whitep)))
             +all-moves+))

(defun best-move (board whitep previous-moves count-map)
  "Determine the most likely chess move from those available to the
   player. Also give the move's associated probability as a second
   value."
  (let (best-move best-probability)
    (loop for move in (valid-moves board whitep)
       for probability = (move-probability move previous-moves count-map)
       when (or (null best-move) (> probability best-probability))
       do (setf best-move move best-probability probability))
    (values best-move best-probability)))
