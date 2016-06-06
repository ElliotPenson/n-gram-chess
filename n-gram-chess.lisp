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
