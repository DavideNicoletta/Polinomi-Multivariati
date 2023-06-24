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

variables(x * y * z, V1).
V1 = [x, y, z].
variables(poly([m(-3, 0, []), m(-4, 1, [v(1, x)])]), V2).
V2 = [x].



monomials:

Il predicato monomials è vero quando Monomials è la
lista dei monomi che appaiono in Poly.

monomials(x + x + x + x, M1).
M1 = [m(4, 1, [v(1, x)])].
monomials(-pippo^123 * pluto, M2).
M2 = [m(-1, 124, [v(123, pippo), v(1, pluto)])].



max_degree:

Il predicato maxdegree è vero quando Degree è il massimo
grado dei monomi che appaiono in Poly.

maxdegree(-a -b ^ 7 -c^2, MAX1).
MAX1 = 7.
maxdegree(a ^ 3 -b + ciao^2, MAX2).
MAX2 = 3.



min_degree:

Il predicato mindegree è vero quando Degree è il minimo
grado dei monomi che appaiono in Poly.

mindegree(-a -b ^ 7 -c^2, MIN1).
MIN1 = 1.
mindegree(-b ^ 0 -b ^ 7 -b^2, MIN2).
MIN2 = 0.



poly_plus:

Il predicato polyplus è vero quando Result è il polinomio somma di
Poly1 e Poly2 ordinato, polyplus richiama il predicato polyplus_call/3
la quale effettua la somma vera e propria.

polyplus(a ^ 3, 3 * b, PP1).
PP1 = poly([m(3, 1, [v(1, b)]), m(1, 3, [v(3, a)])]).
polyplus(poly([m(1, 2, [v(1, a), v(1, b)])]), m(1, 2, [v(1, a), v(1, b)]), PP2).
PP2 = poly([m(2, 2, [v(1, a), v(1, b)])]).



poly_minus:

Il predicato polyminus è vero quando Result è il polinomio
differenza di Poly1 e Poly2 ordinato, polyminus richiama il predicato
polyminus_call/3 la quale effettua la differenza vera e propria.

polyminus(a^ 3, 3 * b, PM1).
PM1 = poly([m(-3, 1, [v(1, b)]), m(1, 3, [v(3, a)])]).
polyminus(m(-1, 1, [v(1, x)]), 3 * x, PM2).
PM2 = poly([m(-4, 1, [v(1, x)])]).



poly_times:

Il predicato polytimes è vero quando Result è il polinomio
risultante, dalla moltiplicazione di Poly1 e Poly2, ordinato, effettua anche la
compatazione  delle variabili e la somma dei monomi simili. Il
predicato polytimes richiama il predicato polytimes_call il quale
effettua il prodotto vero e proprio senza effettuare riduzioni.

polytimes(1, a * b * c, PT1).
PT1 = poly([m(1, 3, [v(1, a), v(1, b), v(1, c)])]).
polytimes(x + y, a + b, PT2).
PT2 = poly([m(1, 2, [v(1, a), v(1, x)]), m(1, 2, [v(1, a), v(1, y)]), m(1, 2, [v(1, b), v(1, x)]), m(1, 2, [v(1, b), v(1, y)])]).



as_monomial:

Il predicato as monomial è vero quando Monomial è il termine che
rappresenta il monomio risultante dal parsing" dell'espressione Expression.
Data la generale confusione sul fatto che questo predicato debba accettare
come primo parametro una forma del tipo (x + x + x), nel nostro caso 

as_monomial(pippo * pippo * pippo * pippo * pippo^5, AM1).
AM1 = m(1, 9, [v(9, pippo)]).
as_monomial(0 * a * b * c * quellochevoglio, AM2).
AM2 = m(0, 0, []).



as_polynomia:

Il predicato as polynomial è vero quando Polynomial è il termine che
rappresenta il polinomio risultante dal parsing dell'espressione Expression.

as_polynomial(pippo + gennaro + 3 * pippo, AP1).
AP1 = poly([m(1, 1, [v(1, gennaro)]), m(4, 1, [v(1, pippo)])]).
as_polynomial(-3 * a ^ 4 +12 * a ^2 * a ^2, AP2).
AP2 = poly([m(9, 4, [v(4, a)])]).



poly_val:

Il predicato poly_val è vero quanto Value contiene il valore del
polinomio Polynomial (che puo' anche essere un monomio), nel punto
n-dimensionale rappresentato dalla lista VariableValues, che contiene
un valore per ogni variabile ottenuta con il predicato variables.

poly_val(x * pippo, [0, 100000], PV1).
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

