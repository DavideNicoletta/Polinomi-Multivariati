function IS-MONOMIAL:

is-monomial (Espressione -> Boolean)
Data in input un'espressione, se è già un Polynomial lo ritorna inalterato, 
se è un Monomial costruirà la struttura Polynomial(Monomial)

CL-USER> (is-monomial '(M 5 7 ((V 3 S) (V 3 T) (V 1 Y))))
T



function IS-POLYNOMIAL:

is-polynomial (Polynomial -> Boolean)
Data in input un'espressione, se è già un Polynomial lo ritorna inalterato, se è un Monomial costruirà la struttura Polynomial(Monomial)

CL-USER> (is-polynomial '(POLY ((M -4 0 NIL) (M 1 1 ((V 1 X))) (M 0 2 ((V 2 X))))))
T



function IS-ZERO:

La funzione ritorna T come Result, quando X è una rappresentazione dello 0 (incluso, ovviamente il caso in cui sia proprio 0).
is-zero (Espressione -> Boolean)

CL-USER> (is-zero 0)
T

CL-USER> (is-zero '(m 0 0 ()))
T

CL-USER> (is-zero '(poly ()))
T



function VAR-POWERS:

var-powers (Monomial -> VP-list)
Data una struttura Monomial, ritorna la lista di varpowers VP-list.

CL-USER> (var-powers '(M 1 7 ((V 3 S) (V 3 T) (V 1 Y))))
((V 3 S) (V 3 T) (V 1 Y))



function VARS-OF:

vars-of (Monomial -> Variables)
Data in input una struttura di tipo Monomial, ritorna la lista di variabili Variables.
Se l'input non è parsato verrà parsato (se possbile).

CL-USER> (vars-of '(M 1 7 ((V 3 S) (V 3 T) (V 1 Y))))		
(S T Y)

CL-USER> (vars-of '(* 42 x a))
(A X)



function MONOMIAL-DEGREE:

monomial-degree (Monomial -> TotalDegree)
Data una struttura Monomial, ritorna il suo grado totale TotalDegree

CL-USER> (monomial-degree '(* 42 (expt a 2) x (expt ciao 3)))
6

CL-USER> (monomial-degree '(M 1 7 ((V 3 S) (V 3 T) (V 1 Y))))		
7



function MONOMIAL-COEFFICIENT:

monomial-coefficient (Monomial -> Coefficient)
Data in input una struttura di tipo Monomial, ritorna il suo coefficiente Coefficient.
Se l'input non è parsato verrà parsato (se possbile).

CL-USER> (monomial-coefficient '(* a t (expt t 4)))
1

CL-USER> (monomial-coefficient '(M 5 7 ((V 3 S) (V 3 T) (V 1 Y))))
5



function COEFFICIENTS:

coefficients (Polynomial -> Coefficients)
La funzione ritorna una lista Coefficients dei coefficienti di Poly.

CL-USER> (coefficients '(+ (* a) (* (expt x 0))))
(1 1)

CL-USER> (coefficients '(POLY ((M -4 0 NIL) (M 1 1 ((V 1 X))) (M 0 2 ((V 2 X))))))
(-4 1 0)



function VARIABLES:

variables (Polynomial -> Variables)
La funzione variables ritorna una lista Variables dei simboli di variabile che appaiono in Poly.

CL-USER> (variables '(+ (* 0 x) (* 42 (expt a 4))))
(A)

CL-USER> (variables '(POLY ((M -4 0 NIL) (M 1 1 ((V 1 Y))) (M 1 2 ((V 2 X))))))
(Y X)



function MONOMIALS:

monomials (Polynomial -> Monomials)
La funzione monomials ritorna la lista dei monomi che appaiono in Poly.

CL-USER> (monomials '(POLY ((M -4 0 NIL))))
((M -4 0 NIL))

CL-USER> (monomials '(POLY ((M -4 0 NIL) (M 1 1 ((V 1 X))) (M 0 2 ((V 2 X))))))
((M -4 0 NIL) (M 1 1 ((V 1 X))) (M 0 2 ((V 2 X))))



function MAX-DEGREE:

max-degree (Polynomial -> Degree)
La funzione max-degree ritorna il massimo grado dei monomi che appaiono in Poly.

CL-USER> (max-degree '(POLY ((M -4 0 NIL) (M 1 1 ((V 1 Y))) (M 1 2 ((V 2 X))))))
2



function MIN-DEGREE:

min-degree (Polynomial -> Degree)
La funzione min-degree ritorna il minimo grado dei monomi che appaiono in Poly

CL-USER> (min-degree '(POLY ((M 1 1 ((V 1 Y))) (M 1 2 ((V 2 X))))))
1



function POLY-PLUS:

poly-plus (Polynomial1, Polynomial2 -> Polynomial)
La funzione poly-plus produce il polinomio somma di Poly1 e Poly2.

CL-USER> (poly-plus '(+ (* a) (* 2 (expt b 2))) '(+ (* 3 a) (* 2 (expt b 2))))
(POLY ((M 4 1 ((V 1 A))) (M 4 2 ((V 2 B)))))



function POLY-MINUS:

poly-minus (Polynomial1, Polynomial2 -> Polynomial)
La funzione poly-minus produce il polinomio differenza di Poly1 e Poly2.

CL-USER> (poly-minus '(POLY ((M 4 2 ((V 2 B))))) '(POLY ((M 2 2 ((V 2 B))))))
(POLY ((M 2 2 ((V 2 B)))))



function POLY-TIMES:

poly-times (Polynomial1, Polynomial2 -> Polynomial)
La funzione poly-times ritorna il polinomio risultante dalla moltiplicazione di Poly1 e Poly2.

CL-USER> (poly-times '(+ (* a) (* 4 (expt b 2))) '(+ (* a) (* 2 (expt b 2))))
(POLY ((M 1 2 ((V 2 A))) (M 6 3 ((V 1 A) (V 2 B))) (M 8 4 ((V 4 B)))))



function AS-MONOMIAL:

as-monomial (Expression -> Monomial)
La funzione as-monomial ritorna la struttura dati (lista) che rappresenta il monomio risultante dal
“parsing” dell’espressione Expression; il monomio risultante deve essere appropriatamente ordinato

CL-USER> (as-monomial '(* 10 (expt a 0) c (expt b 2) (expt a 2) (expt b 2)))
(M 10 7 ((V 2 A) (V 4 B) (V 1 C)))



function AS-POLYNOMIAL:

as-polynomial (Expression -> Polynomial)
La funzione as-polynomial ritorna la struttura dati (lista) che rappresenta il monomio risultante dal
“parsing” dell’espressione Expression; il polinomio risultante deve essere appropriatamente ordinato

CL-USER> (as-polynomial '(+ (* 2 b) (* b) (* 3 c) (* 4 a)))
(POLY ((M 4 1 ((V 1 A))) (M 3 1 ((V 1 B))) (M 3 1 ((V 1 C)))))



function POLY-VAL:

poly-val (Expression, Values -> Number )
La funzione poly-val restituisce il valore Value del polinomio Polynomial 
(che può anche essere un monomio), nel punto n-dimensionale rappresentato dalla lista VariableValues, che contiene un valore per
ogni variabile ottenuta con la funzione variables.

CL-USER> (poly-val '(+ (* a) (* b) (* (expt c 2))) '(1 2 3))
12



function PPRINT-POLYNOMIAL:

pprint-polynomial-call (Monomials -> NIL)
La funzione pprint-polynomial ritorna NIL dopo aver stampato (sullo “standard output”) una rappresentazione tradizionale del termine polinomio
associato a Polynomial. Si puó omettere il simbolo di moltiplicazione.

CL-USER> (pprint-polynomial-call '((M 1 1 ((V 1 A))) (M 1 2 ((V 2 X)))))
(1 * A + 1 * X ^ 2)
