LIBRERIA PER PROLOG

Questa libreria implementa diverse operazioni standard per la
manipolazione di polinomi come ad esempio:
- estrazione dei coefficienti 
- calcolo del grado del polinomio
- valutazione in un punto
- somma 
- moltiplicazione

Rappresentazione di un MONOMIO (RAPPRESENTAZIONE STANDARD):
I MONOMI saranno rappresentati nel seguente modo

m(Coefficient, TotalDegree, VarsPowers).

ESEMPIO
m(1, 0, [])
m(3, 1, [v(1, w)])
m(1, 7, [v(3, s), v(3, t), v(1, y)])


Rappresentazione di una VARIABILE (RAPPRESENTAZIONE STANDARD):
Le VARIABILI saranno rappresentate nel seguente modo
v(Exp, Symbol)

ESEMPIO
v(1, x)


Rappresentazione di un POLINOMIO (RAPPRESENTAZIONE STANDARD):
I POLiNOMI saranno rappresentati nel seguente modo

poly(Monomials) [Un poly e' una lista di monomi]

ESEMPIO
P = poly([m(-4, 0, []), m(1, 2, [v(1, x), v(1, y)]), m(1, 7, [v(3, s), v(3, t), v(1, y)])])






Predicati implementati:

is_monomial:

Il predicato is_monomial è vero se l'oggetto passato è un monomio.

is_monomial(m(1, 7, [v(3, s), v(3, t), v(1, y)])). 
true.



is_polynomial:

Il predicato is_polynomial è vero se l'argomento è una lista di monomi.

is_polynomial(poly([m(-4, 0, []), m(1, 2, [v(1, x), v(1, y)]), m(1, 7, [v(3, s), v(3, t), v(1, y)])])).
true.



is_varpower:

Il predicato is_varpower è vero se l'oggetto passato è un VP.

is_varpower(v(1, x)).
true.



is-zero:

Il predicato is_zero è vero se l'oggetto passato è una rappresentazione dello zero

is_zero(poly([])).
true

is_zero(0).
true

is_zero(m(0, _, _)).
true



coefficients:

Il predicato coefficients è vero quando Coefficients è una lista dei
coefficienti di Poly.

coefficients(m(-1, 1, [v(1, x)]), C1).
C1 = [-1].
coefficients(0 * x -3, C2).
C2 = [-3].



variables:

Il predicato variables è vero quando Variables è una lista
dei simboli di variabile che appaiono in Poly.

variables(x * y * z, V).
V = [x, y, z].




monomials:

Il predicato monomials è vero quando Monomials è la
lista dei monomi che appaiono in Poly.

monomials(x + x + x + x, M).
M = [m(4, 1, [v(1, x)])].



max_degree:

Il predicato max_degree è vero quando Degree è il massimo
grado dei monomi che appaiono in Poly.

max_degree(-a -b ^ 7 -c^2, MAX).
MAX = 7.



min_degree:

Il predicato min_degree è vero quando Degree è il minimo
grado dei monomi che appaiono in Poly.

min_degree(-a -b ^ 7 -c^2, MIN).
MIN = 1.



poly_plus:

Il predicato poly_plus è vero quando Result è il polinomio somma di
Poly1 e Poly2 ordinato, polyplus richiama il predicato polyplus_call/3
la quale effettua la somma vera e propria.

poly_plus(a ^ 3, 3 * b, PP).
PP = poly([m(3, 1, [v(1, b)]), m(1, 3, [v(3, a)])]).



poly_minus:

Il predicato poly_minus è vero quando Result è il polinomio
differenza di Poly1 e Poly2 ordinato, polyminus richiama il predicato
polyminus_call/3 la quale effettua la differenza vera e propria.

poly_minus(a^ 3, 3 * b, PM).
PM = poly([m(-3, 1, [v(1, b)]), m(1, 3, [v(3, a)])]).



poly_times:

Il predicato poly_times è vero quando Result è il polinomio
risultante, dalla moltiplicazione di Poly1 e Poly2, ordinato, effettua anche la
compatazione  delle variabili e la somma dei monomi simili. Il
predicato polytimes richiama il predicato polytimes_call il quale
effettua il prodotto vero e proprio senza effettuare riduzioni.

poly_times(1, a * b * c, PT).
PT = poly([m(1, 3, [v(1, a), v(1, b), v(1, c)])]).



as_monomial:

Il predicato as monomial è vero quando Monomial è il termine che
rappresenta il monomio risultante dal parsing" dell'espressione Expression.
Data la generale confusione sul fatto che questo predicato debba accettare
come primo parametro una forma del tipo (x + x + x), nel nostro caso 

as_monomial(x * x * x * x * x^5, AM).
AM = m(1, 9, [v(9, x)]).




as_polynomia:

Il predicato as_polynomial è vero quando Polynomial è il termine che
rappresenta il polinomio risultante dal parsing dell'espressione Expression.

as_polynomial(x + y + 3 * x, AP).
AP = poly([m(4, 1, [v(1, x)]), m(1, 1, [v(1, y)])]).



poly_val:

Il predicato poly_val è vero quanto Value contiene il valore del
polinomio Polynomial (che puo' anche essere un monomio), nel punto
n-dimensionale rappresentato dalla lista VariableValues, che contiene
un valore per ogni variabile ottenuta con il predicato variables.

poly_val(x * y, [0, 1], PV1).
PV1 = 0.
poly_val(poly([m(3, 1, [v(1, x)]), m(1, 2, [v(2, a)])]), [-1, -2], PV2).
PV2 = -5.



pprint_polynomial:

Il predicato pprint_polynomial risulta vedo dopo aver stampato
una rappresentazione tradizionale del termine
polinomio associato a Polynomial. Si puo' omettere il simbolo di
moltiplicazione.

pprint_polynomial(poly([m(-4, 0, []), m(1, 2, [v(1, x), v(1, y)]), m(1, 7, [v(3, s), v(3, t), v(1, y)])])).
-4 + x * y + s^3 * t^3 * y

