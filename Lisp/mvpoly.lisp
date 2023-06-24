;;;; -*- Mode: Lisp -*-
;;;; 858101 Nicoletta Davide



;;; as-monomial/1
;; Parses a monomial
;; NB: per il concetto di atomo in CL, (as-monomial '-x)
;; prende -x come simbolo di variabile

(defun as-monomial (expr)
  (compress-vars-in-monomial (sort-monomial (as-monomial-start expr))))


;;; as-polynomial/1
;; Parses the input as a polynomial, sorts and reduces it

(defun as-polynomial (expr)
  (if (is-monomial expr) (parse-polynomial expr)
    (append (list 'poly)
	    (list
	     (remove-zero
	      (sum-similar-monos-in-poly
	       (sort-poly (as-polynomial-execute expr))))))))



;;; is-monomial/1
;; Returns true if m is a monomial

(defun is-monomial (m)
  (and (listp m)
       (eq 'm (first m))
       (let ((mtd (monomial-degree m))
             (vps (var-powers m)))
	 (and (integerp mtd)
	      (>= mtd 0)
	      (listp vps)
	      (every #'is-varpower vps)))))


;;; is-polynomial/1
;; T if p is a polynomial

(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))


;;; is-zero/1
;; La funzione ritorna T come Result, quando X è una rappresentazione dello 0 (incluso, ovviamente il caso
;; in cui sia proprio 0).
(defun is-zero (mono)
  (cond
   ((and (numberp mono)(eq mono 0)) T) 
   ((and (eq (first mono) 'm) (eq (second mono) 0)) T)
   ((and (eq (first mono) 'poly) (eq (second mono) '())) T)
   (t nil)))


;;; var-powers/1
;; Data una struttura Monomial, ritorna la lista di varpowers VP-list.

(defun var-powers (mono)
  (if (and (= (length mono) 4) (eq 'm (first mono)))
      (let ((vps (fourth mono)))
	(if (listp vps)
	    vps
	  nil))
    (if (is-polynomial mono) 
        nil
      (let* ((parsed-mono (as-monomial mono)) (vps (fourth parsed-mono)))
	(if (listp vps)
	    vps
          nil)))))



;;; vars-of/1
;; Data una struttura Monomial, ritorna la lista di variabili Variables.

(defun vars-of (mono)
  (if (not (and (equal (first mono) 'm) (= (length mono) 4)))
      (vars-of (as-monomial mono))
    (apply #'append
	   (let ((vps (var-powers mono)))
	     (append (list (mapcar (lambda (x) (third x)) vps)))))))


;;; monomial-degree/1
;; Data una struttura Monomial, ritorna il suo grado totale TotalDegree

(defun monomial-degree (mono)
  (if (and (= (length mono) 4) (eq 'm (first mono)))
      (let ((mtd (third mono)))
        (if (>= mtd 0) mtd nil))
    (if (is-polynomial mono) 
        nil
      (let* ((parsed-mono (as-monomial mono)) (mtd (third parsed-mono)))
	(if (>= mtd 0) mtd nil)))))



;;; monomial-coefficient/1
;; Data una struttura Monomial, ritorna il suo coefficiente Coefficient.

(defun monomial-coefficient (mono)
  (if (null mono) 0
    (if (and (= (length mono) 4) (eq 'm (first mono)))
	(let ((coeff (second mono)))
	  (if (numberp coeff) coeff nil))
      (let* ((parsed-mono (as-monomial mono)) (coeff (second parsed-mono)))
	(if (numberp coeff) coeff nil)))))



;;; coefficients/1
;; La funzione coefficients ritorna una lista Coefficients dei – ovviamente – coefficienti di Poly.

(defun coefficients (p)
  (let* ((parsed-p (parse-polynomial p)) 
         (monomials (monomials parsed-p)))
    (if (null monomials) 
        '(0)
      (mapcar 'monomial-coefficient monomials))))


;;; variables/1
;; La funzione variables ritorna una lista Variables dei simboli di variabile che appaiono in Poly.

(defun variables (p)
  (let ((parsed-p (parse-polynomial p)))
    (remove-duplicates (mapcar #'varpower-symbol
			       (apply #'append
				      (mapcar #'var-powers
					      (monomials parsed-p)))))))



;;; monomials/1
;; La funzione monomials ritorna la lista dei monomi che appaiono in Poly.

(defun monomials (p)
  (if (equal (first p) 'poly) 
      (first (rest p)) 
    (monomials (parse-polynomial p))))



;;; max-degree/1
;; La funzione max-degree ritorna il massimo grado dei monomi che appaiono in Poly.

(defun max-degree (p)
  (let* ((parsed-p (parse-polynomial p)))
    (monomial-degree (first (last (monomials parsed-p))))))


;;; min-degree/1
;; La funzione min-degree ritorna il minimo grado dei monomi che appaiono in Poly

(defun min-degree (p)
  (let* ((parsed-p (parse-polynomial p)))
    (monomial-degree (first (monomials parsed-p)))))





;;; poly-plus/2
;; La funzione poly-plus produce il polinomio somma di Poly1 e Poly2.

(defun poly-plus (poly1 poly2)
  (let ((p1 (parse-polynomial poly1)) (p2 (parse-polynomial poly2)))
    (append (list 'poly)
            (list (remove-zero
                   (sort-poly
                    (sum-similar-monos-in-poly
                     (sort-poly (append (monomials p1)
                                        (monomials p2))))))))))


;;; poly-minus/2
;; La funzione poly-minus produce il polinomio differenza di Poly1 e Poly2.

(defun poly-minus (poly1 poly2)
  (let ((p1 (parse-polynomial poly1)) (p2 (parse-polynomial poly2)))
    (append (list 'poly)
            (list (remove-zero
                   (sort-poly
                    (sum-similar-monos-in-poly
                     (sort-poly (append (monomials p1)
                                        (change-sign
                                         (monomials p2)))))))))))


;;; poly-times/2
;; La funzione poly-times ritorna il polinomio risultante dalla moltiplicazione di Poly1 e Poly2.

(defun poly-times (poly1 poly2)
  (append (list 'poly)
          (list (remove-zero
                 (sort-poly (sum-similar-monos-in-poly
                             (poly-times-call
                              (monomials (parse-polynomial poly1))
                              (monomials (parse-polynomial poly2)))))))))


;;; poly-val/2
;; La funzione poly-val restituisce il valore Value del polinomio Polynomial 
;; (che può anche essere un monomio), nel punto n-dimensionale rappresentato dalla lista VariableValues, che contiene un valore per
;; ogni variabile ottenuta con la funzione variables.

(defun poly-val (poly value)
  (if (not (equal 'poly (first poly))) (poly-val (parse-polynomial poly) value)
    (if (listp value)
	(let* ((polyParsed (parse-polynomial poly))
	       (vars (variables polyParsed))
	       (alternate (new-pairlis vars value)))
	  (if (null vars) (monomial-coefficient (second poly))
	    (let* ((monos (monomials polyParsed))
		   (monos-with-value
		    (substitute-vars-in-mono monos alternate)))
	      (evaluate-monos monos-with-value))))
      nil)))


;;; pprint-polynomial/1
;; La funzione pprint-polynomial ritorna NIL dopo aver stampato (sullo “standard output”) una rappresentazione tradizionale del termine polinomio
;; associato a Polynomial. Si puó omettere il simbolo di moltiplicazione.

(defun pprint-polynomial (poly)
  (format t "~a"
	  (format nil "~a"
		  (pprint-polynomial-call
		   (second (parse-polynomial poly))))))

;;; varpower-power/1
;; Returns the exponent from a variable (v Exp VarSymbol)

(defun varpower-power (vp)
  (let ((pow (second vp)))
    (if (numberp pow) pow)))


;;; varpower-symbol/1
;; Returns the varsymbol from a variable (v Exp VarSymbol)

(defun varpower-symbol (vp)
  (let ((vs (third vp)))
    (cond ((and
	    (atom vs)
	    (not (numberp vs))) vs)
	  (T nil))))


;;; is-varpower/1
;; T if vp is a list of var-powers

(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
             (v (varpower-symbol vp))
	     )
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))


;;; remove-zero/1
;; Removes every monomial with coefficient = 0 from a list of monos

(defun remove-zero (monos)
  (if (null monos) nil
    (let ((coeff (monomial-coefficient (first monos))))
      (if (= coeff 0)
	  (remove-zero (rest monos))
	(append (list (first monos)) (remove-zero (rest monos)))))))









;;; is-number/1
;; If (eval expression) is a number, returns it. Else NIL

(defun is-number (expression)
  (let ((result 
         (handler-case 
             (eval expression)
	   (error () nil)
	   (warning () nil))))
    (if (numberp result) 
        result 
      nil)))


;;; is-power-not-parsed/1
;; True if expr is an expression to be parsed

(defun is-power-not-parsed (expr)
  (if (not (listp expr)) nil
    (if (and (equal (first expr) 'expt) (symbolp (second expr))
	     (numberp (third expr)))
	T NIL)))


;;; parse-power/1
;; Parses an expression (expt VAR EXP) into the form (v EXP VAR)

(defun parse-power (expr)
  (if (is-power-not-parsed expr)
      (if (not (eq (third expr) 0))
	  (list 'm 1 (third expr)
		(list 'v (third expr)
		      (second expr)))
	(list 'm 1 '0 nil))
    nil))


;;; parse-power-negative-coeff/1
;; Parses an expression (expt VAR EXP) into the form (v EXP VAR) and
;; manages a negative coefficient

(defun parse-power-negative-coeff (expr)
  (if (is-power-not-parsed expr)
      (list 'm -1 (third expr) (list 'v (third expr) (second expr))) nil))


;;; is-operator/1
;; True if expr is + or - or * or /

(defun is-operator (expr)
  (if (or (eql expr '*) (eql expr '/) (eql expr '-) (eql expr '+))
      T NIL))


;;; sort-monomial/1
;; Sorts the variables in a monomial by lexicographical order

(defun sort-monomial (mono)
  (let ((new-var-powers (copy-list (var-powers mono))))
    (append (list (first mono) (second mono) (third mono))
	    (list (stable-sort new-var-powers 'string< :key 'third)))))


;;; compare-var-powers/2
;; Compares the variables in monomials with the same TD

(defun compare-var-powers (vars1 vars2)
  (cond ((null vars1) (not (null vars2)))
	((null vars2) nil)
	(t
         (let ((v1 (first vars1)) (v2 (first vars2)))
           (cond
	    ((string< (third v1) (third v2)) t)
	    ((string> (third v1) (third v2)) nil)
	    ((and (equal (third v1) (third v2)) (= (second v1) (second v2)))
	     (compare-var-powers (rest vars1) (rest vars2)))
	    (t (< (second (first vars1)) (second (first vars2)))))))))


;;; compare-degrees/2
;; Compares the degrees of the monomials in a poly

(defun compare-degrees (first-mono rest-monos)
  (when (not (null first-mono))
    (let ((degrees
	   (list (monomial-degree first-mono)
		 (monomial-degree rest-monos))))
      (cond ((null first-mono) (not (null rest-monos)))
            ((null rest-monos) nil)
            ((= (first degrees) (second degrees))
	     (compare-var-powers (var-powers first-mono) (var-powers rest-monos)))
            (t (< (first degrees) (second degrees)))))))


;;; sort-poly/2
;; Sorts a polynomial by degree and lexicographical order

(defun sort-poly (monos)
  (let ((poly-copied (copy-list monos)))
    (stable-sort poly-copied #'compare-degrees)))


;;; build-coefficient/1
;; Evaluates the coefficient of a monomial

(defun build-coefficient (expr)
  (if (null expr) 1
    (if (is-number (first expr))
	(* 1 (eval (first expr)) (build-coefficient (rest expr)))
      (* 1 (build-coefficient (rest expr))))))


;;; build-var-powers/2
;; Builds the VPs of a monomial

(defun build-var-powers (expr td)
  (let ((head (first expr)) (tail (rest expr)))
    (cond ((and (listp head)
		(not (null head))
		(not (eq (third head) 0))
		(equal (first head) 'expt))
           (append (build-var-powers tail (+ (eval td) (eval (third head))))
		   (list (list 'v (third head) (second head)))))
          ((and (listp head)
		(not (null head))
		(eq (third head) 0)
		(equal (first head) 'expt))
           (append (build-var-powers tail (+ (eval td) (eval (third head))))
		   nil))
          ((and (symbolp head) (not (null head)))
           (append (build-var-powers tail (+ 1 (eval td)))
		   (list (list 'v 1 head))))
          ((numberp (eval head)) (build-var-powers tail td))
          ((null head) (list td)))))


;;; as-monomial-start/1
;; Parses the input as an unsorted monomial

(defun as-monomial-start (expr)
  (cond ((is-number expr) (list 'm (eval expr) 0 nil))
        ((atom expr) (list 'm 1 1 (list (list 'v 1 expr))))
        (t (let ((head (first expr)) (tail (rest expr)))
             (if (is-operator head)
                 (cond ((equal head '-)
                        (if (listp (second expr))
                            (parse-power-negative-coeff (second expr))
			  (list 'm -1 1 (list 'v 1 (second expr)))))
                       ((equal head '*)
                        (if (eql (build-coefficient tail) 0) (list 'm 0 0 nil)
			  (let ((vps (build-var-powers tail 0)))
			    (append (list 'm) (list (build-coefficient tail))
				    (list (first vps)) (list (rest vps))))))
                       ((equal head '+)
                        nil))
	       (if (is-power-not-parsed head)
		   (parse-power head)
		 (list 'm 1 1 (list (list 'v 1 head)))))))))


;;; as-polynomial-execute/1
;; Parses the input as a polynomial

(defun as-polynomial-execute (expr)
  (when (not (null expr))
    (if (atom expr) (list (as-monomial expr))
      (let ((head (first expr)) (tail (rest expr)))
	(if (is-operator head)
	    (if (equal head '+)
		(as-polynomial-execute tail)
	      (list (as-monomial expr)))
	  (if (and (listp expr) (not (null tail)))
	      (append (list (as-monomial head)) (as-polynomial-execute tail))
	    (list (as-monomial head))))))))


;;; check-equal-variables/1
;; This function checks if the variables in the monomial are equal

(defun check-equal-variables (expr)
  (let ((variables1 (fourth (first expr))) (variables2 (fourth (second expr))))
    (if (equal variables1 variables2) T NIL)))


;;; sum-similar-monos-in-poly/1
;; This function sums the similar monomials in a polynomial

(defun sum-similar-monos-in-poly (monos)
  (cond ((null monos) nil)
        ((null (second monos)) monos)
        (t
         (let* ((mono1 (first monos))
		(mono2 (second monos))
		(c1 (monomial-coefficient mono1))
		(c2 (monomial-coefficient mono2))
		(td (monomial-degree mono1))
		(vps1 (var-powers mono1))
		(vps2 (var-powers mono2)))
           (if (not (equal vps1 vps2))
	       (append (list mono1)
		       (sum-similar-monos-in-poly (rest monos)))
	     (sum-similar-monos-in-poly
	      (append (list (list 'm (+ c1 c2) td vps1))
		      (rest (rest monos)))))))))


;;; compress-vars-in-monomial/1
;; This function sums the exponents of similiar VPs in a monomial

(defun compress-vars-in-monomial (mono)
  (if (null (var-powers mono)) mono
    (let ((vps (var-powers mono))
	  (c (monomial-coefficient mono))
	  (td (monomial-degree mono)))
      (append (list 'm c td) (list (compress-vps vps))))))


;;; compress-vps/1
;; This function sums the exponents of similiar VPs

(defun compress-vps (vps)
  (if (null vps) nil
    (if (null (second vps)) vps
      (let* ((vp1 (first vps))
	     (vp2 (second vps))
	     (expt1 (varpower-power vp1))
	     (expt2 (varpower-power vp2))
	     (var1 (varpower-symbol vp1))
	     (var2 (varpower-symbol vp2))
	     (tail (rest (rest vps))))
	(if (not (null tail))
	    (if (not (null vp2))
		(if (equal var1 var2)
		    (compress-vps (append
				   (list (list 'v (+ (eval expt1)
						     (eval expt2))
					       var1))
				   tail))
		  (append (list (list 'v expt1 var1))
			  (compress-vps (rest vps)))))
	  (if (equal var1 var2) (list (list 'v (+ (eval expt1)
						  (eval expt2))
					    var1))
	    (append (list (list 'v expt1 var1))
		    (list (list 'v expt2 var2)))))))))


;;; new-pairlis/2
;; This function creates a list with elements from list1 and list2 alternated

(defun new-pairlis (list1 list2)
  (if (null list1) nil
    (if (null list2) nil
      (append (list (list (first list1) (first list2)))
	      (new-pairlis (rest list1) (rest list2))))))


;;; change-sign/1
;; This function changes the sign of the coefficients

(defun change-sign (monos)
  (if (null monos) nil
    (let* ((mono1 (first monos))
	   (c1 (second mono1))
	   (td (third mono1))
	   (var-powers (fourth mono1)))
      (append (list (list 'm (- c1) td var-powers))
	      (change-sign (rest monos))))))



;;; parse-polynomial/1
;; Checks whether the input is a Poly. If not, the function parses the input

(defun parse-polynomial (poly)
  (cond ((is-polynomial poly)
	 (append (list 'poly) (sort-poly (list (monomials poly)))))
	((is-monomial poly)
	 (append (list 'poly) (list (list poly))))
	((if (or (atom poly) (equal '* (first poly)))
	     (parse-polynomial (as-monomial poly))
	   (as-polynomial poly)))
        (t nil)))




;;; substitute-var-in-vp/2
;; This function substitutes variable in a vp with a number
;; (evaluation point of the poly)

(defun substitute-var-in-vp (vp alternate)
  (if (null alternate) nil
    (let* ((var (third vp))
	   (var-a (first (first alternate)))
	   (value (second (first alternate)))
	   (tail (rest alternate))
	   (expt (second vp)))
      (if (and (null var) (null expt))
	  (list 'v 0 0)
	(if (eq var var-a)
	    (list 'v expt value)
	  (substitute-var-in-vp vp tail))))))


;;; substitute-vars-in-vps/2
;; This predicate calls the method that performs variable substitution

(defun substitute-vars-in-vps (vps alternate)
  (let* ((head (first vps)) (tail (rest vps)))
    (if (not (null tail))
	(append (list (substitute-var-in-vp head alternate))
		(substitute-vars-in-vps tail alternate))
      (list (substitute-var-in-vp head alternate)))))


;;; substitute-vars-in-mono/2
;; This function creates a new monomial by substituing the variables with
;; the corresponding values appearing in the list "alternate"

(defun substitute-vars-in-mono (monos alternate)
  (let* ((head (first monos))
	 (tail (rest monos))
	 (vps (var-powers head))
	 (coef (second head))
         (td (third head))
	 (vps-a (substitute-vars-in-vps vps alternate)))
    (if (not (null tail))
	(append (list (list 'm coef td vps-a))
		(substitute-vars-in-mono tail alternate))
      (list (list 'm coef td vps-a)))))


;;; evaluate-monos/1, evaluate-vps/1
;; They caluculate the value of the monomials that compose a polynomial
;; in a certain point

(defun evaluate-monos (monos)
  (let* ((head (first monos))
	 (tail (rest monos))
	 (coef (second head))
	 (vps (var-powers head))
	 (vps-a (evaluate-vps vps)))
    (if (not (null tail))
	(+ (* coef vps-a) (evaluate-monos tail))
      (* coef vps-a))))

(defun evaluate-vps (vps)
  (let* ((head (first vps))
	 (tail (rest vps))
	 (exp (second head))
	 (base (third head)))
    (if (not (null tail))
	(* (expt base exp) (evaluate-vps tail))
      (expt base exp))))




;;; poly-times-call/2
;; This function multiplies two polynomials

(defun poly-times-call (monos1 monos2)
  (if (or (null monos1) (null monos2)) nil
    (let* ((head1 (first monos1))
	   (head2 (first monos2))
	   (tail1 (rest monos1))
	   (tail2 (rest monos2)))
      (append (list (mono-times head1 head2))
	      (poly-times-call (list head1) tail2)
	      (poly-times-call tail1 monos2)))))


;;; mono-times/2
;; This function multiplies two monomials

(defun mono-times (mono1 mono2)
  (cond ((null mono1) mono2)
        ((null mono2) mono1)
        (t (let ((c1 (monomial-coefficient mono1))
                 (c2 (monomial-coefficient mono2))
                 (td1 (monomial-degree mono1))
                 (td2 (monomial-degree mono2))
                 (vps1 (var-powers mono1))
                 (vps2 (var-powers mono2)))
             (if (or (= 0 c1) (= 0 c2)) (list 'm 0 0 nil)
	       (append (list 'm
			     (* c1 c2)
			     (+ td1 td2)
			     (multiply-variables vps1 vps2))))))))


;;; multiply-variables/2
;; This function multiplies two VPs

(defun multiply-variables (vps1 vps2)
  (cond ((null vps1) vps2)
        ((null vps2) vps1)
        (t (let* ((vp1 (first vps1))
                  (vp2 (first vps2))
                  (exp1 (varpower-power vp1))
                  (exp2 (varpower-power vp2))
                  (var1 (varpower-symbol vp1))
                  (var2 (varpower-symbol vp2)))
             (if (equal var1 var2)
                 (append (list (list 'v (+ exp1 exp2) var1))
                         (multiply-variables (rest vps1) (rest vps2)))
	       (if (string>= var1 var2)
		   (append (list (list 'v exp2 var2))
			   (multiply-variables vps1 (rest vps2)))
		 (append (list (list 'v exp1 var1))
			 (multiply-variables (rest vps1) vps2))))))))




;;; pprint-polynomial-call/1
;; This function prints a traditional form of poly

(defun pprint-polynomial-call (mono)
  (let ((m1 (first mono)) (c2 (second (second mono))))
    (if (not (null c2))
        (if (> c2 0)
            (append (pprint-polynomial-call-coefficients m1)
		    (list '+)
		    (pprint-polynomial-call (rest mono)))
	  (append (pprint-polynomial-call-coefficients m1)
		  (pprint-polynomial-call (rest mono))))
      (append (pprint-polynomial-call-coefficients m1)))))


;;; pprint-polynomial-call-coefficients/1
;; This function prints the coefficient of a mono

(defun pprint-polynomial-call-coefficients (m1)
  (let ((c1 (second m1)) (v&p (fourth m1)))
    (if (equal v&p nil)
        (append (list c1))
      (append (list c1)
	      (list '*)
	      (pprint-polynomial-call-variables v&p)))))


;;; pprint-polynomial-call-variables/1
;; This function prints variables and powers of a mono

(defun pprint-polynomial-call-variables (var-power)
  (if (null var-power) nil
    (let ((exp (second (first var-power))) (var (third (first var-power))))
      (if (equal (rest var-power) nil)
	  (if (= exp 1)
	      (append (list var)) (append (list var '^ exp)))
	(if (= exp 1)
	    (append (list var '*)
		    (pprint-polynomial-call-variables (rest var-power)))
	  (append (list var '^ exp '*)
		  (pprint-polynomial-call-variables
		   (rest var-power))))))))


;;; end of file -- mvpoly.lisp




