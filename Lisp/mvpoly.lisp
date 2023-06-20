;;;; -*- Mode: Lisp -*-
;;;; 858101 Nicoletta Davide

;;; is-zero/1
(defun is-zero(mono)
  (if (eq (first mono) 'M)
      (eq (second mono) 0)
      (if (eq (first mono) 'POLY)
	  (eq (second mono) '())
	  nil)))


