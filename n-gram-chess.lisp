;;;; n-gram-chess.lisp
;;;; Author: Elliot Penson

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :knight-owl))

(defpackage :n-gram-chess
  (:nicknames :ngc)
  (:use :common-lisp :knight-owl))

(in-package :n-gram-chess)
