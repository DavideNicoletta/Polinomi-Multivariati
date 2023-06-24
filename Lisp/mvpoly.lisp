;;;; -*- Mode: Lisp -*-
;;;; 858101 Nicoletta Davide



;;; as-monomial/1
;; La funzione as-monomial ritorna la struttura dati (lista)
;; che rappresenta il monomio risultante dal
;; “parsing” dell’espressione Expression

(defun as-monomial (expr)
  (reduce-monomial (sort-monomial (as-monomial-start expr))))


;;; as-polynomial/1
;; La funzione as-polynomial ritorna la struttura dati (lista)
;; che rappresenta il monomio risultante dal
;; “parsing” dell’espressione Expression

(defun as-polynomial (expr)
  (if (is-monomial expr) (parse-polynomial expr)
    (append (list 'poly)
	    (list
	     (remove-zero
	      (sum-similar-monos
	       (sort-poly (as-polynomial-execute expr))))))))



;;; is-monomial/1
;; La funzioen ritorna true se m è un monomio

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
;; La funzioen ritorna true se p è un polinomio

(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))


;;; is-zero/1
;; La funzione ritorna T come Result, quando X è una rappresentazione
;; dello 0 (incluso, ovviamente il caso in cui sia proprio 0).
(defun is-zero (X)
  (cond
   ((and (numberp X)(eq X 0)) T) 
   ((and (eq (first X) 'm) (eq (second X) 0)) T)
   ((and (eq (first X) 'poly) (eq (second X) '())) T)
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
;; La funzione coefficients ritorna una lista Coefficients
;; dei  coefficienti di Poly.

(defun coefficients (p)
  (let* ((parsed-p (parse-polynomial p)) 
         (monomials (monomials parsed-p)))
    (if (null monomials) 
        '(0)
      (mapcar 'monomial-coefficient monomials))))


;;; variables/1
;; La funzione variables ritorna una lista Variables dei simboli di
;; variabile che appaiono in Poly.

(defun variables (p)
  (let ((parsed-p (parse-polynomial p)))
    (remove-duplicates (mapcar #'varpower-symbol
			       (apply #'append
				      (mapcar #'var-powers
					      (monomials parsed-p)))))))



;;; monomials/1
;; La funzione monomials ritorna la lista
;; dei monomi che appaiono in Poly.

(defun monomials (p)
  (if (equal (first p) 'poly) 
      (first (rest p)) 
    (monomials (parse-polynomial p))))



;;; max-degree/1
;; La funzione max-degree ritorna il massimo grado
;; dei monomi che appaiono in Poly.

(defun max-degree (p)
  (let* ((parsed-p (parse-polynomial p)))
    (monomial-degree (first (last (monomials parsed-p))))))


;;; min-degree/1
;; La funzione min-degree ritorna il minimo grado
;; dei monomi che appaiono in Poly

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
                    (sum-similar-monos
                     (sort-poly (append (monomials p1)
                                        (monomials p2))))))))))


;;; poly-minus/2
;; La funzione poly-minus produce il polinomio differenza di Poly1 e Poly2.

(defun poly-minus (poly1 poly2)
  (let ((p1 (parse-polynomial poly1)) (p2 (parse-polynomial poly2)))
    (append (list 'poly)
            (list (remove-zero
                   (sort-poly
                    (sum-similar-monos
                     (sort-poly (append (monomials p1)
                                        (change-sign
                                         (monomials p2)))))))))))


;;; poly-times/2
;; La funzione poly-times ritorna il polinomio risultante
;; dalla moltiplicazione di Poly1 e Poly2.

(defun poly-times (poly1 poly2)
  (append (list 'poly)
          (list (remove-zero
                 (sort-poly (sum-similar-monos
                             (poly-times-call
                              (monomials (parse-polynomial poly1))
                              (monomials (parse-polynomial poly2)))))))))


;;; poly-val/2
;; La funzione poly-val restituisce il valore Value
;; del polinomio Polynomial (che può anche essere un monomio),
;; nel punto n-dimensionale rappresentato dalla lista VariableValues,
;; che contiene un valore per ogni variabile ottenuta con la funzione variables.

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
;; La funzione pprint-polynomial ritorna NIL dopo aver stampato
;; (sullo “standard output”) una rappresentazione tradizionale
;; del termine polinomio associato a Polynomial.

(defun pprint-polynomial (poly)
  (format t "~a"
	  (format nil "~a"
		  (pprint-polynomial-execute
		   (second (parse-polynomial poly))))))

;;; varpower-power/1
;; La funzione ritorna l'espondente di una variabile

(defun varpower-power (vp)
  (let ((pow (second vp)))
    (if (numberp pow) pow)))


;;; varpower-symbol/1
;; La funzione ritorna il simbolo della variabile

(defun varpower-symbol (vp)
  (let ((vs (third vp)))
    (cond ((and
	    (atom vs)
	    (not (numberp vs))) vs)
	  (T nil))))


;;; is-varpower/1
;; La funzione ritorna true quando vp è una lista di variabili

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
;; La funzione rimuove i monomi che hanno coefficiente zero

(defun remove-zero (monos)
  (if (null monos) nil
    (let ((coeff (monomial-coefficient (first monos))))
      (if (= coeff 0)
	  (remove-zero (rest monos))
	(append (list (first monos)) (remove-zero (rest monos)))))))



;;; is-number/1
;; Data in input un'espressione, se è un numero ritorna il numero stesso,
;; se è una funzione composta calcola il risultato.

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
;; Data in input un'espressione, se tale espressione è una lista del tipo
;; (expt varsymbol number) allora ritorna true.

(defun is-power-not-parsed (expr)
  (if (not (listp expr)) nil
    (if (and (equal (first expr) 'expt) (symbolp (second expr))
	     (numberp (third expr)))
	T NIL)))


;;; parse-power/1
;; Data in input un'espressione, se tale espressione è una lista del tipo
;; (expt varsymbol number) allora tale espressione verrà
;; ordinata in formato (MONO 1 TD (VP)).


(defun parse-power (expr)
  (if (is-power-not-parsed expr)
      (if (not (eq (third expr) 0))
	  (list 'm 1 (third expr)
		(list 'v (third expr)
		      (second expr)))
	(list 'm 1 '0 nil))
    nil))


;;; parse-power-negative-coeff/1
;; Data in input un'espressione, se tale espressione è una lista del tipo
;; (expt varsymbol number)  allora tale espressione viene
;; ordinata in formato (MONO -1 TD (VP)).


(defun parse-power-negative-coeff (expr)
  (if (is-power-not-parsed expr)
      (list 'm -1 (third expr) (list 'v (third expr) (second expr))) nil))


;;; is-operator/1
;;La funzione ritorna true se operator è un operatore algebrico (*, / , + , -).

(defun is-operator (operator)
  (if (or (eql operator '*) (eql operator '/)
	  (eql operator '-) (eql operator '+))
      T nil))


;;; sort-monomial/1
;; Data in inputuna struttura del tipo Monomial, restituisce tale Monomio
;; ordinato secondo ordine lessicografico

(defun sort-monomial (mono)
  (let ((new-var-powers (copy-list (var-powers mono))))
    (append (list (first mono) (second mono) (third mono))
	    (list (stable-sort new-var-powers 'string< :key 'third)))))


;;; order-power/2
;; Date in input due strutture di tipo VP, restituisce true se la variabile
;; di VP1 , in ordine alfabetico, viene prima rispetto a quella di VP2.

(defun order-power (vars1 vars2)
  (cond ((null vars1) (not (null vars2)))
	((null vars2) nil)
	(t
         (let ((v1 (first vars1)) (v2 (first vars2)))
           (cond
	    ((string< (third v1) (third v2)) t)
	    ((string> (third v1) (third v2)) nil)
	    ((and (equal (third v1) (third v2)) (= (second v1) (second v2)))
	     (order-power (rest vars1) (rest vars2)))
	    (t (< (second (first vars1)) (second (first vars2)))))))))


;;; order-degrees/2
;; Date in input due strutture di tipo Monomial, restituisce true
;; se il TD di Monomial1 è minore del TD di Monomial2

(defun order-degrees (first-mono rest-monos)
  (when (not (null first-mono))
    (let ((degrees
	   (list (monomial-degree first-mono)
		 (monomial-degree rest-monos))))
      (cond ((null first-mono) (not (null rest-monos)))
            ((null rest-monos) nil)
            ((= (first degrees) (second degrees))
	     (order-power (var-powers first-mono)
				 (var-powers rest-monos)))
            (t (< (first degrees) (second degrees)))))))


;;; sort-poly/2
;; Date in input una strutture di tipo Monomials, ovvero i monomi
;; che compongono un Polynomial, restituisce la lista di Monomials
;; ordinati lessicograficamente.

(defun sort-poly (monos)
  (let ((poly-copied (copy-list monos)))
    (stable-sort poly-copied #'order-degrees)))


;;; get-coefficient/1
;; Date in input una generica Expression ritorna il Coefficient
;; risultante dal parsing di tale Expression.

(defun get-coefficient (expression)
  (if (null expression) 1
    (if (is-number (first expression))
	(* 1 (eval (first expression)) (get-coefficient (rest expression)))
      (* 1 (get-coefficient (rest expression))))))


;;; get-var-powers/2
;; Date in input una generica Expression ritorna la struttura
;; (TD (VPS)) risultante dal parsing di tale Expression.

(defun get-var-powers (expr td)
  (let ((head (first expr)) (tail (rest expr)))
    (cond ((and (listp head)
		(not (null head))
		(not (eq (third head) 0))
		(equal (first head) 'expt))
           (append (get-var-powers tail (+ (eval td) (eval (third head))))
		   (list (list 'v (third head) (second head)))))
          ((and (listp head)
		(not (null head))
		(eq (third head) 0)
		(equal (first head) 'expt))
           (append (get-var-powers tail (+ (eval td) (eval (third head))))
		   nil))
          ((and (symbolp head) (not (null head)))
           (append (get-var-powers tail (+ 1 (eval td)))
		   (list (list 'v 1 head))))
          ((numberp (eval head)) (get-var-powers tail td))
          ((null head) (list td)))))


;;; as-monomial-start/1
;; Date in input un'espressione , (se possbile) verrà parsata nella struttura
;; Monomial senza però tener conto di riduzioni possibili e ordinamento.

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
                        (if (eql (get-coefficient tail) 0) (list 'm 0 0 nil)
			  (let ((vps (get-var-powers tail 0)))
			    (append (list 'm) (list (get-coefficient tail))
				    (list (first vps)) (list (rest vps))))))
                       ((equal head '+)
                        nil))
	       (if (is-power-not-parsed head)
		   (parse-power head)
		 (list 'm 1 1 (list (list 'v 1 head)))))))))


;;; as-polynomial-execute/1
;; Date in input un'espressione , (se possbile) verrà parsata nella struttura
;; Polynomial senza però tener conto di riduzioni possibili e ordinamento.

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


;;; is-equal-variables/1
;; Dati in input due Monomi , ritorna TRUE se i due monomi
;; hanno le stesse variabili.

(defun is-equal-variables (expr)
  (let ((variables1 (fourth (first expr))) (variables2 (fourth (second expr))))
    (if (equal variables1 variables2) T NIL)))


;;; sum-similar-monos/1
;; Data in input la lista di Monomial che compongono un Poy,
;; ritorna la lista dei Monomial ridotti.

(defun sum-similar-monos (monos)
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
		       (sum-similar-monos (rest monos)))
	     (sum-similar-monos
	      (append (list (list 'm (+ c1 c2) td vps1))
		      (rest (rest monos)))))))))


;;; reduce-monomial/1
;; Data in input una struttura di tipo Monomial (già ordinata),
;; ritorna il Monomial ridotto.

(defun reduce-monomial (mono)
  (if (null (var-powers mono)) mono
    (let ((vps (var-powers mono))
	  (c (monomial-coefficient mono))
	  (td (monomial-degree mono)))
      (append (list 'm c td) (list (reduce-vps vps))))))


;;; reduce-vps/1
;; Data in input una struttura di tipo VPS , ovvero una lista
;; di VP (già ordinata), ritorna la VPS ridotta.

(defun reduce-vps (vps)
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
		    (reduce-vps (append
				   (list (list 'v (+ (eval expt1)
						     (eval expt2))
					       var1))
				   tail))
		  (append (list (list 'v expt1 var1))
			  (reduce-vps (rest vps)))))
	  (if (equal var1 var2) (list (list 'v (+ (eval expt1)
						  (eval expt2))
					    var1))
	    (append (list (list 'v expt1 var1))
		    (list (list 'v expt2 var2)))))))))


;;; new-pairlis/2
;; Date in input due liste List1 e List2 , ritorna una lista
;; alternata di elementi di List1 e List2.

(defun new-pairlis (list1 list2)
  (if (null list1) nil
    (if (null list2) nil
      (append (list (list (first list1) (first list2)))
	      (new-pairlis (rest list1) (rest list2))))))


;;; change-sign/1
;; Data in input la lista di Monomial che compongono un Polynomial,
;; ritorna la lista dei Monomi con il Coefficiente invertito di segno.

(defun change-sign (monos)
  (if (null monos) nil
    (let* ((mono1 (first monos))
	   (c1 (second mono1))
	   (td (third mono1))
	   (var-powers (fourth mono1)))
      (append (list (list 'm (- c1) td var-powers))
	      (change-sign (rest monos))))))



;;; parse-polynomial/1
;; Data in input un'espressione, se è già un Polynomial lo ritorna inalterato,
;; se è un Monomial costruirà la struttura Polynomial(Monomial)

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
;; Data in input una struttura di tipo VP e una lista Alternate contenente
;; liste di coppie (VARIABILE VALORE), sostituisce la VARIABILE di VP
;; con il VALORE presente nella coppia Alternate ogni qualvolta la VARIABILE
;; di Alternate è uguale alla VARIABILE di VP

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
;; Data in input una struttura di tipo VPS , ovvero una lista di VP , 
;; e una lista Alternate contenente liste di coppie (VARIABILE VALORE),
;; scorre tutte le VP  della VPS e sostituisce la VARIABILE di VP con
;; il VALORE presente nella coppia Alternate ogni qualvolta la VARIABILE
;; di Alternate è uguale alla VARIABILE di VP.

(defun substitute-vars-in-vps (vps alternate)
  (let* ((head (first vps)) (tail (rest vps)))
    (if (not (null tail))
	(append (list (substitute-var-in-vp head alternate))
		(substitute-vars-in-vps tail alternate))
      (list (substitute-var-in-vp head alternate)))))


;;; substitute-vars-in-mono/2
;; Data in input una struttura di tipo Monomial e una lista Alternate
;; contenente liste  di coppie (VARIABILE VALORE), scorre tutte le VP
;; contenute nella VPS del Monomial  e sostituisce la VARIABILE di VP
;; con il VALORE presente nella coppia Alternate, ogni qualvolta la
;; VARIABILE di Alternate è uguale alla VARIABILE di VP.

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


;;; evaluate-monos/1
;; Data in input una struttura di tipo Monomials, ovvero la
;; lista di Monomial che compongono  un Polynomial, con valori
;; delle variabili già sostituiti mediante l'utilizzo di
;; SUBSTITUTE-VARS-IN-MONO, ritorna il valore della valutazione dei Monomials.

(defun evaluate-monos (monos)
  (let* ((head (first monos))
	 (tail (rest monos))
	 (coef (second head))
	 (vps (var-powers head))
	 (vps-a (evaluate-vps vps)))
    (if (not (null tail))
	(+ (* coef vps-a) (evaluate-monos tail))
      (* coef vps-a))))


;;;evaluate-vps/1
;; Data in input una struttura di tipo VPS, ovvero la lista di
;; VP che compongono un Monomial, con valori delle variabili già
;; sostituiti mediante l'utilizzo di SUBSTITUTE-VARS-IN-VP, ritorna
;; il valore della valutazione delle VP di VPS.

(defun evaluate-vps (vps)
  (let* ((head (first vps))
	 (tail (rest vps))
	 (exp (second head))
	 (base (third head)))
    (if (not (null tail))
	(* (expt base exp) (evaluate-vps tail))
      (expt base exp))))




;;; poly-times-call/2
;; Dati in input Monomials1 e Monomials2 , ovvero le liste di Monomial
;; che compongono rispettivamente i due polinomi Polynomial1 e Polynomial2
;; da moltiplicare, ritorna la lista di Monomial (non ridotta e non ordinata)
;; risultante dal prodotto di Monomials1 e Monomials2.

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
;; Dati in input Monomial1 e Monomial2 , due strutture di tipo Monomial,
;; ritorna il Monomial risultante dalla moltiplicazione di
;; Monomial1 e Monomial2.

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
;; Dati in input VPS1 e VPS2 (ordinate), due strutture di tipo lista di VP,
;; ritorna la VPS risultante dalla  moltiplicazione di VPS1 e VPS2.

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




;;; pprint-polynomial-execute/1
;; Dato in input una struttura Monomials, ovvero la lista di Monomial che
;; compongono un Polynomials,  stampa sullo STDOUTPUT Monomials.

(defun pprint-polynomial-execute (mono)
  (let ((m1 (first mono)) (c2 (second (second mono))))
    (if (not (null c2))
        (if (> c2 0)
            (append (pprint-polynomial-execute-coefficients m1)
		    (list '+)
		    (pprint-polynomial-execute (rest mono)))
	  (append (pprint-polynomial-execute-coefficients m1)
		  (pprint-polynomial-execute (rest mono))))
      (append (pprint-polynomial-execute-coefficients m1)))))


;;; pprint-polynomial-execute-coefficients/1
;; Dato in input una struttura Monomial  stampa sullo STDOUTPUT i coefficienti
;; e le  variabili del Monomial dato in input.

(defun pprint-polynomial-execute-coefficients (m1)
  (let ((c1 (second m1)) (v&p (fourth m1)))
    (if (equal v&p nil)
        (append (list c1))
      (append (list c1)
	      (list '*)
	      (pprint-polynomial-execute-variables v&p)))))


;;; pprint-polynomial-execute-variables/1
;; Dato in input una struttural VPS stampa sullo STDOUTPUT le variabili del
;; Monomial dato in input.

(defun pprint-polynomial-execute-variables (var-power)
  (if (null var-power) nil
    (let ((exp (second (first var-power))) (var (third (first var-power))))
      (if (equal (rest var-power) nil)
	  (if (= exp 1)
	      (append (list var)) (append (list var '^ exp)))
	(if (= exp 1)
	    (append (list var '*)
		    (pprint-polynomial-execute-variables (rest var-power)))
	  (append (list var '^ exp '*)
		  (pprint-polynomial-execute-variables
		   (rest var-power))))))))


;;; end of file -- mvpoly.lisp




