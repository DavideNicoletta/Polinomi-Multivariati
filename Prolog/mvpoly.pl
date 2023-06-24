%%%% -*- Mode: Prolog -*-
%%%% 858101 Nicoletta Davide



%%% is_zero/1
is_zero(poly([])) :-
    !.
is_zero(m(0, _, _)) :-
    !.
is_zero(0) :-
    !.


%%% is_monomial/1
%%% Il predicato is_monomial e' vero se l'oggetto passato e' un monomio.
is_monomial(m(_C, TD, VPs)) :-
    integer(TD),
    TD >= 0,
    is_list(VPs),
    foreach(member(V, VPs), is_varpower(V)),
    sum_degrees(VPs, TD).

is_monomial(poly(_)) :- false.




%%% sum_degrees/2
%%% Il predicato sum_degrees_variables e' vero se TotalDegree e' la somma
%%% di tutti gli esponenti delle VP.
sum_degrees([], 0) :- !.
sum_degrees([v(Exponent, _Variable) | Vs], TotalDegree) :-
    sum_degrees(Vs, TotalDegree2),
    !,
    TotalDegree is Exponent + TotalDegree2.



%%% is_varpower/1
%%% Il predicato is_varpower e' vero se l'oggetto passato e' un VP.
is_varpower(v(Power, VarSymbol)) :-
    integer(Power),
    Power >= 0,
    atom(VarSymbol).




%%% as_monomial/2
%%% Il predicato as_monomial e' vero quando Monomial e' la
%%% rappresentazione standard del primo predicato ordinato e compattato.
as_monomial(Expression, Monomial) :-
    monomial_sorted(Expression, StartMonomial),
    reduce_monomial(StartMonomial, Monomial).


%%% monomial_sorted/2
%%% True se il secondo argomento è un monomio analizzato e ordinato 
%%% a partire da un'Espressione passata come primo argomento
monomial_sorted(Expression, m(C, TD, VPs)) :-
		    monomial_start(Expression, m(C, TD, VPs2)),
		    sort(2, @=<, VPs2, VPs).

%%% monomial_start/2
%%% True quando Mono e' la
%%% rappresentazione standard di Expression la quale rappresenta la
%%% versione tradizionale di un monomio.
monomial_start(0, m(0, 0, [])) :-
    !.
monomial_start(_ + _, _) :-
    false.
monomial_start(-Monomial, m(NC, TD, VPs)) :-
    !,
    monomial_start(Monomial, m(C, TD, VPs)),
    !,
    NC is -C.
monomial_start(SingleVariable, m(1, 1, [v(1, SingleVariable)])) :-
    atom(SingleVariable),
    !.
monomial_start(SingleVariable ^ Expression,
	       m(1, Expression, [v(Expression, SingleVariable)])) :-
    Expression \= 0,
    atom(SingleVariable),
    !,
    integer(Expression),
    !.
monomial_start(SingleVariable ^ Expression, m(1, 0, [])) :-
    atom(SingleVariable),
    !,
    integer(Expression),
    !.
monomial_start(Head * Tail, m(C, TD, [v(1, Tail) | VPs])) :-
    atom(Tail),
    !,
    monomial_start(Head, m(C, TD1, VPs)),
    TD is TD1 + 1.
monomial_start(Head * A ^ 0, m(C, TD, VPs)) :-
    atom(A),
    !,
    monomial_start(Head, m(C, TD1, VPs)),
    TD is TD1.
monomial_start(Head * A ^ B, m(C, TD, [v(B, A) | VPs])) :-
    number(B),
    !,
    atom(A),
    !,
    monomial_start(Head, m(C, TD1, VPs)),
    TD is TD1 + B.
monomial_start(Coefficent, m(C, 0, [])) :-
    Coefficent \= 0,
    !,
    arithmetic_expression_value(Coefficent, C).


%%% reduce_monomial/2
%%% True e' vero quando MonomialReduce rappresenta
%%% il parametro Monomial in seguito alla compattazione delle variabili.
reduce_monomial(Monomial, MonomialReduce) :-
    !,
    reduce_monomial_execute(Monomial, MonomialReduce).


%%% reduce_monomial_execute/2
reduce_monomial_execute(m(0, _, _), m(0, 0, [])) :-
			    !.
reduce_monomial_execute(m(C, 0, []), m(C, 0, [])) :-
    !.
reduce_monomial_execute(m(C, TD, [v(Exponent, Variable)]),
			m(C, TD, [v(Exponent, Variable)])) :-
    !.
reduce_monomial_execute(m(C, TD,
			 [v(Exponent1, Variable), v(Exponent2, Variable) | VPs]),
			 m(C, TD, VPsReduced)) :-
			    !,
			    X is Exponent1 + Exponent2,
			    !,
			    reduce_monomial(m(C, TD, [v(X, Variable) | VPs]),
					    m(C, TD, VPsReduced)).
reduce_monomial_execute(m(C, TD, [v(Exponent1, Var), v(Exponent2, DiffVar) | VPs]),
		     m(C, TD, [v(Exponent1, Var) | VPsReduced])) :-
    !,
    reduce_monomial(m(C, TD, [v(Exponent2, DiffVar) | VPs]),
		    m(C, TD, VPsReduced)).


%%% is_polynomial/1
%%% Il predicato is_polynomial e' vero se l'argomento e' una lista di monomi.
is_polynomial(poly(Monomials)) :-
    is_list(Monomials),
    !,
    foreach(member(M, Monomials), is_monomial(M)).
is_polynomial(Monomial) :-
    is_monomial(Monomial).



%%% as_polynomial/2
%%% Il predicato as_polynomial e' vero quando Polynomial rappresenta la
%%% rappresentazione standard del primo parametro ordinata e con la
%%% eliminazione dei monomi con il  coefficiente a 0.
as_polynomial(Expression, Polynomial) :-
    polynomial_sorted(Expression, PolynomialZero),
    remove_zero(PolynomialZero, Polynomial).


%%% polynomial_sorted/2
%%% Analizza e ordina un polinomio, quindi somma i monomi simili in esso contenuti.
polynomial_sorted(m(0, _, _), poly([])) :-
    !.
polynomial_sorted(m(C, TD, VPs2), poly([m(C, TD, VPs)])) :-
		      is_monomial(m(C, TD, VPs2)),
		      !,
		      sort(2, @=<, VPs2, VPs).
polynomial_sorted(Expression, poly(Monomials)) :-
    polynomial_start(Expression, poly(Monomial)),
    sort_polynomials(poly(Monomial), poly(SortedMonomials)),
    sum_monomials(poly(SortedMonomials), poly(Monomials)).


%%% sort_polynomials/2
%%% True quando poly(SortedMonomials) rappresenta poly(Monomials) in seguito all'ordinamento lessicografico e' in base al grado
sort_polynomials(poly(Monomials), poly(SortedMonomials)) :-
    remove_zero(poly(Monomials), poly(MonomialsZero)),
    predsort(compare_monomials, MonomialsZero, SortedMonomials).


%%% remove_zero/2
%%% True quando il secondo polinomio rappresenta il primo
%%% polinomio privato di tutti i monomi con coefficiente pari a 0
remove_zero(poly([]), poly([])) :-
    !.

remove_zero(poly([m(0, _, _) | Tail]), poly(Tail2)) :-
    !,
    remove_zero(poly(Tail), poly(Tail2)).

remove_zero(poly([m(C, TD, VPs) | Tail]), poly([m(C, TD, VPs) | Tail2])) :-
		!,
		remove_zero(poly(Tail), poly(Tail2)).


%%% polynomial_start/2
polynomial_start(Head + Tail, poly(Parsed)) :-
    as_monomial(Tail, ParsedTail),
    polynomial_start(Head, poly(ParsedHead)),
    !,
    append([ParsedTail], ParsedHead, Parsed).
polynomial_start(Head - Tail, poly(Parsed)) :-
    as_monomial(-Tail, ParsedTail),
    polynomial_start(Head, poly(ParsedHead)),
    !,
    append([ParsedTail], ParsedHead, Parsed).
polynomial_start(Monomial, poly([ParsedMono])) :-
    !,
    as_monomial(Monomial, ParsedMono).


%%% sum_monomials/2
%%% True quando il secondo polinomio rappresenta il 
%%% il primo polinomio con tutti i monomi simili sommati.
sum_monomials(poly([]), poly([])) :-
    !.
sum_monomials(poly([X]), poly([X])) :-
    !.
sum_monomials(poly([m(C, TD, VPs), m(D, TD, VPs) | Tail1]), poly(Tail2)) :-
    !,
    X is C + D,
    !,
    sum_monomials(poly([m(X, TD, VPs) | Tail1]), poly(Tail2)).
sum_monomials(poly([A, B | Tail1]), poly([A | Tail2])) :-
    !,
    sum_monomials(poly([B | Tail1]), poly(Tail2)).


%%% coefficients/2
%%% Il predicato coefficients e' vero quando Coefficients e' una lista dei
%%% coefficienti di Poly.

coefficients(poly([]), [0]) :-
    !.
coefficients(Polynomial, Coefficients) :-
    red_sort_polynomial(Polynomial, PolynomialParsed),
    !,
    get_polynomial_coefficients(PolynomialParsed, Coefficients).


%%% is_mono_parse/1
is_mono_parse(m(_, _, _)) :-
    !.

%%% red_sort_polynomial/2
%%% True quando ParsedPolynomial rappresenta il primo parametro 
%%% in seguito all'ordinamento.
red_sort_polynomial(Monomial, ParsedPolynomial) :-
    is_monomial(Monomial),
    is_mono_parse(Monomial),
    !,
    as_polynomial(Monomial, ParsedPolynomial).
red_sort_polynomial(Polynomial, ParsedPolynomial) :-
    is_polynomial(Polynomial),
    !,
    sort_polynomials(Polynomial, Sort),
    sum_monomials(Sort, Sum),
    remove_zero(Sum, ParsedPolynomial).
red_sort_polynomial(Polynomial, ParsedPolynomial) :-
   as_polynomial(Polynomial, ParsedPolynomial).


%%% get_polynomial_coefficients/2
get_polynomial_coefficients(poly([]), []) :-
    !.
get_polynomial_coefficients(poly(Monomials), Coefficients) :-
    get_polynomial_coefficients_execute(poly(Monomials), Coefficients).


%%% get_polynomial_coefficients_execute/2
get_polynomial_coefficients_execute(poly([]), []) :-
    !.
get_polynomial_coefficients_execute(poly([Head | Rest]), [R | RestCoef]) :-
    get_coefficient_mono(Head, R),
    get_polynomial_coefficients_execute(poly(Rest), RestCoef).
    

%%% get_coefficient_mono/2
%%% True quando C è il coefficiente del monomio
get_coefficient_mono(m(C, _, _), C) :-
    !.
    

%%% compare_monomials/3
%%% True quando, in base al predicato > o <, il primo monomio risultera' essere maggiore 
%%% o minore rispetto al secondo monomio

compare_monomials(<, m(_C1, TD, VPs1), m(_C2, TD, VPs2)) :-
    compare_variables(<, VPs1, VPs2),
    !.
compare_monomials(>, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
    TD1 > TD2, !.
compare_monomials(>, m(_C1, TD, VPs1), m(_C2, TD, VPs2)) :-
    compare_variables(>, VPs1, VPs2),
    !.
compare_monomials(<, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
    TD1 < TD2, !.



%%% compare_variables/3
%%% Il predicato compare_variables e' vero quando, in base al predicato >
%%% o <, VPS1 risultera' essere maggiore( o minore) rispetto a VPS2.
compare_variables(>, [], _) :- !.
compare_variables(<, _, []) :- !.
compare_variables(<, v(_, Var1), v(_, Var2)) :-
    Var1 @< Var2,
    !.
compare_variables(>, v(_, Var1), v(_, Var2)) :-
    Var1 @> Var2,
    !.
compare_variables(<, v(Exp1, Var), v(Exp2, Var)) :-
    Exp1 < Exp2,
    !.
compare_variables(>, v(Exp1, Var), v(Exp2, Var)) :-
    Exp1 > Exp2,
    !.
compare_variables(<, [v(Exp, Var) | Vs1] , [v(Exp, Var) | Vs2]) :-
    !,
    compare_variables(<, Vs1, Vs2).
compare_variables(<, [v(Exp1, Var) | _] , [v(Exp2, Var) | _]) :-
    Exp1 < Exp2,
    !.
compare_variables(>, [v(Exp, Var) | Vs1] , [v(Exp, Var) | Vs2]) :-
    !,
    compare_variables(>, Vs1, Vs2).
compare_variables(>, [v(Exp1, Var) | _] , [v(Exp2, Var) | _]) :-
    Exp1 > Exp2,
    !.
compare_variables(<, [v(_, Var1) | _Vs1] , [v(_, Var2) | _Vs2]) :-
    Var1 @< Var2,
    !.
compare_variables(>, [v(_, Var1) | _Vs1] , [v(_, Var2) | _Vs2]) :-
    Var1 @> Var2,
    !.


%%% variables/2
%%% True quando Variables e' una lista
%%% dei simboli di variabile che appaiono in Poly.
variables(poly([]), []) :-
    !.
variables(Poly, Variables) :-
    red_sort_polynomial(Poly, PolynomialParsed),
    !,
    polynomial_variables(PolynomialParsed, VariablesList),
    get_symbol_var(VariablesList, List),
    !,
    sort(List, Variables).


%%% polynomial_variables/2
%%% True se il secondo argomento si unifica con la lista delle variabili del
%%% monomi che compongono il polinomio passato come primo argomento, ordinati per
%%% ordine lessicografico
polynomial_variables(poly([]), []) :-
    !.
polynomial_variables(poly(Monomials), Variables) :-
    polynomial_variables_execute(poly(Monomials), VariablesT),
    append(VariablesT, VariablesN),
    sort(2, @=<, VariablesN, Variables).


%%% polynomial_variables_execute/2
polynomial_variables_execute(poly([]), []) :-
    !.
polynomial_variables_execute(poly([Head | Rest]), [R | VariableList]) :-
    get_monomial_var(Head, R),
    polynomial_variables_execute(poly(Rest), VariableList).


%%% get_monomial_var/2
%%% True quando il secondo parametro rappresenta la lista di variabili e potenze che appaiono nel
%%% monomio passato come primo parametro.
get_monomial_var(m(_, _, VPs), VPs) :-
    !.


%%% get_symbol_var/2
%%% True se SymbolVar e' il simbolo di variabile della VP in questione.

get_symbol_var([], []) :-
    !.
get_symbol_var([v(_, SymbolVar) | VPs], [SymbolVar | VPs2]) :-
    !,
    get_symbol_var(VPs, VPs2).


%%% monomials/2
%%% True quando Monomials e' la lista dei monomi che appaiono in Poly
monomials(poly([]), []) :-
    !.
monomials(Poly, Monomials) :-
    red_sort_polynomial(Poly, poly(Parsed)),
    !,
    sort_polynomials(poly(Parsed), poly(Monomials)).


%%% pprint_polynomial/1
%%% True dopo aver stampato una rappresentazione tradizionale del termine
%%% polinomio associato a Polynomial.
pprint_polynomial(Poly) :-
    red_sort_polynomial(Poly, PolynomialParsed),
    !,
    pprint_execute(PolynomialParsed),
    nl.

%%% pprint_execute/1
pprint_execute(poly([])) :-
    write("Polinomio vuoto").
pprint_execute(poly([m(C, 0, [])])) :-
    write(C),
    !.
pprint_execute(poly([m(1, _, VPs)])) :-
    !,
    print_VarPower(VPs).
pprint_execute(poly([m(C, _, VPs)])) :-
    !,
    write(C),
    !,
    write(" * "),
    !,
    print_VarPower(VPs).
pprint_execute(poly([Head | Tail])) :-
    pprint_execute(poly([Head])),
    write(" + "),
    !,
    pprint_execute(poly(Tail)).
			

%%% print_VarPower/1
print_VarPower([]) :-
    !.
print_VarPower([v(1, Variable)]) :-
    !,
    write(Variable).
print_VarPower([v(Exponent, Variable)]) :-
    Exponent \= 1,
    !,
    write(Variable),
    write(^),
    write(Exponent).
print_VarPower([v(Exponent, Variable) | VPs]) :-
    print_VarPower([v(Exponent, Variable)]),
    write(" * "),
    print_VarPower(VPs).


%%% max_degree/2
%%% True quando Degree e' il massimo grado dei monomi che appaiono in Poly.
max_degree(poly([]), 0) :-
    !.
max_degree(Poly, Degree) :-
    red_sort_polynomial(Poly, poly(ParsedPolynomial)),
    !,
    last_monomial(ParsedPolynomial, m(_, DegreeLastMonomial, _)),
    !,
    DegreeLastMonomial >= 0,
    Degree is DegreeLastMonomial.
    


%%% last_monomial/2
%%% True quando Max e' l'ultimo monomio presente nella lista di Monomi 
%%% passata come primo parametro.
last_monomial([], m(0, 0, [])) :-
    !.
last_monomial([X], X) :-
    !.
last_monomial([_ | Xs], Max) :-
    last_monomial(Xs, Max).


%%% min_degree/2
%%% True quando Degree e' il minimo grado dei monomi che appaiono in Poly.
min_degree(poly([]), 0) :-
    !.
min_degree(Poly, Degree) :-
    red_sort_polynomial(Poly, poly([m(_, Degree, _) | _])),
    !.

%%% poly_plus/3
%%% True quando Result e' il polinomio somma di Poly1 e Poly2 ordinato
poly_plus(Poly1, Poly2, Result) :-
    red_sort_polynomial(Poly1, Parsed1),
    red_sort_polynomial(Poly2, Parsed2),
    poly_plus_minus_execute(Parsed1, Parsed2, Result).

%%% poly_plus_minus_execute/3
poly_plus_minus_execute(poly([]), poly([]), poly([])) :-
    !.
poly_plus_minus_execute(poly([]), poly(Monomial), poly(SortMonomial)) :-
    sort_polynomials(poly(Monomial), poly(SortMonomial)),
    !.
poly_plus_minus_execute(poly(Monomial), poly([]), poly(SortMonomial)) :-
    sort_polynomials(poly(Monomial), poly(SortMonomial)),
    !.
poly_plus_minus_execute(poly(FirstMonomial),
			poly(SecondMonomial), poly(Result)) :-
    append(FirstMonomial, SecondMonomial, X),
    sort_polynomials(poly(X), poly(Y)),
    sum_monomials(poly(Y), poly(W)),
    all_monomial_reduce(poly(W), poly(J)),
    remove_zero(poly(J), poly(Result)).
									    
%%% all_monomial_reduce/2
%%% quando il secondo polinomio rappresenta il primo
%%% polinomio in seguito alla compatazione delle variabili.
all_monomial_reduce(poly([]), poly([])) :-
    !.
all_monomial_reduce(poly([Head | Tail]), poly([HeadR | TailR])) :-
    reduce_monomial(Head, HeadR),
    all_monomial_reduce(poly(Tail), poly(TailR)).

%%% poly_minus/3
%%% True quando Result e' il polinomio differenza di Poly1 e Poly2 ordinato
poly_minus(Poly1, Poly2, Result) :-
    red_sort_polynomial(Poly1, Parsed1),
    red_sort_polynomial(Poly2, Parsed2),
    reverse_polynomial(Parsed2, ReverseParsed2),
    poly_plus_minus_execute(Parsed1, ReverseParsed2, Result).


%%% reverse_polynomial/2
%%% Truequando il secondo polinomio rappresenta il primo polinomio con tutti i 
%%% coefficienti dei vari monomi cambiati di segno.
reverse_polynomial(poly([]), poly([])) :-
    !.
reverse_polynomial(poly([m(C, TD, VPs) | Mono]),
		   poly([m(NC, TD, VPs) | Reverse])) :-
    NC is -C,
    !,
    reverse_polynomial(poly(Mono), poly(Reverse)).


%%% poly_times/3
%%% True quando Result e' il polinomio risultante, dalla moltiplicazione 
%%% di Poly1 e Poly2, ordinato, effettua anche la compatazione  delle 
%%% variabili e la somma dei monomi simili. 
poly_times(Poly1, Poly2, Result) :-
    red_sort_polynomial(Poly1, Parsed1),
    !,
    red_sort_polynomial(Poly2, Parsed2),
    !,
    poly_times_execute(Parsed1, Parsed2, ExecuteResult),
    sort_polynomials(ExecuteResult, Sort),
    all_monomial_reduce(Sort, ReduceResult),
    sum_monomials(ReduceResult, Sum),
    remove_zero(Sum, Result).


%%% poly_times_execute/3

poly_times_execute(poly([]), poly([]), poly([])) :-
    !.
poly_times_execute(poly([]), poly(_), poly([])) :-
    !.
poly_times_execute(poly(_), poly([]), poly([])) :-
    !.
poly_times_execute(poly([Head | Tail]), poly([Head2 | Tail2]),
		   poly([X | Tail3])) :-
    mulTimes(Head, Head2, X),
    poly_times_execute(poly([Head]), poly(Tail2), poly(Tail4)),
    poly_times_execute(poly(Tail), poly([Head2 | Tail2]), poly(Tail5)),
    append(Tail4, Tail5, Tail3).




%%% mulTimes/3
%%% Truequando il terzo polinomio e' uguale al prodotto 
%%% tra il primo e il secondo polinomio
mulTimes(m(0, _, _), m(_, _, _), m(0, 0, [])) :- !.
mulTimes(m(_, _, _), m(0, _, _), m(0, 0, [])) :- !.
mulTimes(M, [], M) :- !.
mulTimes([], M, M) :- !.
mulTimes(m(C1, TD1, VPs1), m(C2, TD2, VPs2), m(C, TD, VPs)) :-
    C is C1 * C2, !,
    TD is TD1 + TD2, !,
    multiply_variables(VPs1, VPs2, VPs).
		       
		       
			  
%%% multiply_variables/3
%%% This predicate sums the similar variables in a Monomial
%%% True quando VPSRIS rappresenta il risultato del prodotto tra VPS1 e VPS2.
multiply_variables([], [], []) :- !.
multiply_variables(V, [], V) :- !.
multiply_variables([], V, V) :- !.
multiply_variables([v(Exp1, Var) | Vs1], [v(Exp2, Var) | Vs2],
		               [v(Exp, Var) | Vs]) :-
    Exp is Exp1 + Exp2,
    !,
    multiply_variables(Vs1, Vs2, Vs).
multiply_variables([v(Exp1, Var1) | Vs1], [v(Exp2, Var2) | Vs2],
		               [v(Exp2, Var2) | Vs]) :-
    Var1 @>= Var2,
    !,
    multiply_variables([v(Exp1, Var1) | Vs1], Vs2, Vs).
multiply_variables([v(Exp1, Var1) | Vs1], [v(Exp2, Var2) | Vs2],
		               [v(Exp1, Var1) | Vs]) :-
    Var2 @>= Var1,
    !,
    multiply_variables(Vs1, [v(Exp2, Var2) | Vs2], Vs).			    
			    

%%% poly_val/3
%%% True quanto Value contiene il valore del polinomio Polynomial 
%%% (che puo' anche essere un monomio), nel punto n-dimensionale rappresentato
%%% dalla lista VariableValues, che contiene un valore per ogni variabile 
%%% ottenuta con il predicato variables.
polyval(Polynomial, VariableValues, Value) :-
    polyvalCall(Polynomial, VariableValues, Value).

%%%% end of file -- mvpoly.pl
    
