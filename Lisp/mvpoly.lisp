;;;; -*- Mode: Lisp -*-
;;;; 858101 Nicoletta Davide

;;; is-zero/1
(defun is-zero(mono)
  (if (eq (first mono) 'm)
      (eq (second mono) 0)
      (if (eq (first mono) 'POLY)
	  (eq (second mono) '())
	  nil)
      ))


(defun is-monomial (m)
  (and (listp m)
       ( ))
       (let ((mtd (monomial-total-degree m))
             (vps (monomial-vars-and-powers m)))
	 (and (integerp mtd)
	      (>= mtd 0)
	      (or (listp vps) (not vps)) ; verificare condizione con monomi con varpowe nil
	      (every #'is-varpower vps))))


;;; monomial-total-degree/1
(defun monomial-total-degree (m)
  (let ((mtd (third m)))
    (cond ((> mtd 0) mtd)
	  (t nil))))


;;; monomial-vars-and-powers/1
(defun monomial-vars-and-powers (m)
  (let ((vps (fourth  m)))
    (cond ((listp vps) vps)
	  (t nil))))

	     
;;; is-varpower/1
(defun is-varpower(vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
	     (v (varpower-symbol vp))
	     )
	 (and (integerp p)
	      (>= p 0)
	      (symbolp v)))))

	 
;;; varpower-power/1
(defun varpower-power (vp)
  (let ((pow (second vp)))
    (if (numberp pow) pow)))


;;; varpower-symbol/1
(defun varpower-symbol (vp)
  (let ((symbol (third vp)))
    (cond (
	   (and
	    (atom symbol)
	    (not (numberp symbol))
	    ) symbol)
	  (t nil))))



;;; is-polynomial/1
(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))


;;; monomials/1
(defun monomials (p)
  (second p))

;;; end of file -- mvpoly.lisp


;;; monomials/1
;; Returns the list of all the monomials in a poly

#|
(defun monomials (p)
  (if (equal (first p) 'poly) 
      (first (rest p)) 
    (monomials (to-polynomial p))))


;;; to-polynomial/1
;; Checks whether the input is a Poly. If not, the function parses the input

(defun to-polynomial (poly)
  (cond ((is-polynomial poly)
	 (append (list 'poly) (sort-poly (list (monomials poly)))))
	((is-monomial poly)
	 (append (list 'poly) (list (list poly))))
	((if (or (atom poly) (equal '* (first poly)))
	     (to-polynomial (as-monomial poly))
	     (as-polynomial poly)))
        (t (error "Not a valid input! EXPECTED: [Poly] or [Monos]"))))
|#




