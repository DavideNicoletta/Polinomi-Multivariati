%%%% -*- Mode: Prolog -*-
%%%% 858101 Nicoletta Davide



%%% is_monomial/1
is_monomial(poly(_)) :-
    false.
is_monomial(m(_C, TD, VPs)) :-
    integer(TD),
    TD >= 0,
    is_list(VPs)
    foreach(member(V, VPs), is_varpower(V)),
    sum_degrees(VPs, TD).

%%% sum_degrees/2
sum_degrees([], 0) :- !.
sum_degrees([v(Exponent, _Variable) | Vs], TotalDegree) :-
    sum_degrees(Vs, TotalDegree2), !,
    TotalDegree is Exponent + TotalDegree2.
    
%%% is_varpower/1
is_varpower(v(Power, VarSymbol)) :-
    integer(Power),
    Power >= 0,
    atom(VarSymbol).

%%% as_monomial/2
%%% as_monomial(Expression, Monomial)
as_monomial(Expression, Monomial) :-
    monomial_sorted(Expression, StartMonomial),
    reduce_monomial(StartMonomial, Monomial).


%%% monomial_sorted/2
monomial_sorted(Expression, m(C, TD, VPs)) :-
		    monomial_start(Expression, m(C, TD, VPs2)),
		    sort(2, @=<, VPs2, VPs).

%%% monomial_start/2
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
monomial_start(Head * Tail, m(C, TD, m(C, TD, [v(1, Tail) | VPs])) :-
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
reduce_monomial(Monomial, MonomialReduce) :-
    !,
    reduce_monomial_execute(Monomial, MonomialReduce).

%%% reduce_monomial_execute/2
reduce_monomial_execute(m(0, _, _), m(0, 0, []) :-
			    !.
reduce_monomial_execute(m(C, 0, []), m(C, 0, [])) :-
    !.
reduce_monomial_execute(m(C, TD, [v(Exponent, Variable)]),
			m(C, TD, [v(Exponent, Variable)])) :-
    !.
reduce_monomial_execute(m(C, TD,
			 [v(Exponent1, Variable), v(Exponent2, Variable) | VPs],
			 m(C, TD, VPReduce)) :-
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
%%% is_polynomials(poly[Monomials])
%%% true when Monomials is an list composed of monomails
is_polynomial(poly(Monomials)) :-
    is_list(Monomials),
    foreach(member(M, Monomials), is_monomial(M)).
is_polynomial(Monomial) :-
    is_monomial(Monomial).



%%% as_polynomial/2
as_polynomial(Expression, Polynomial) :-
    polynomial_sorted(Expression, PolynomialZero),
    remove_zero(PolynomialZero, Polynomial).

%%% polynomial_sorted/2
polynomial_sorted(m(0, _, _), poly([])) :-
    !.
polynomial_sorted(m(C, TD, VPs2), poly([m(C, TD, VPs])) :-
		      is_monomial(m(C, TD, VPs2)),
		      !,
		      sort(2, @=<, VPs2, VPs).
polynomial_sorted(Expression, poly(Monomials)) :-
    polynomial_start(Expression, poly(Monomial)),
    sort_polynomial(poly(Monomial), poly(SortedMonomials)),
    sum_monomials(poly(SortedMonomials), poly(Monomials)).


%%% sort_polynomials/2
sort_polynomials(poly(Monomials), poly(SortedMonomials)) :-
    remove_zero(poly(Monomials), poly(MonomialsZero)),
    predsort(compare_monomials, MonomialsZero, SortedMonomials).


%%% remove_zero/2
remove_zero(poly([]), poly([])) :-
    !.
remove_zero(poly([m(0, _, _) | Tail]), poly(Tail2)) :-
    !,
    remove_zero(poly(Tail), poly(Tail2)).
remove_zero(poly([m(C, TD, VPs) | Tail], poly([m(C, TD, VPs) | Tail2])) :-
		!,
		remove_zero(poly(Tail), poly(Tail2)).


%%% polynomial_start/2
polynomial_start(Head + Tail, poly(Parsed)) :-
    as_monomial(Tail, ParsedTail),
    polynomila_start(Head, poly(ParsedHead)),
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
%%% coefficients(Poly, Coefficients)
%%% true whene Coefficients is a list composed of
%%% the coefficients of polinomial

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
red_sort_polynomail(Monomial, ParsedPolynomial) :-
    is_monomial(Monomial),
    is_mono_parse(Monomial),
    !,
    as_polynomial(Monomial, ParsedPolynomial).
red_sort_polynomial(Polynomial, ParsedPolynomial) :-
    is_polynomial(Polynomial),
    sort_polynomial(Polynomial, Sort),
    sum_monomials(Sort, Sum),
    remove_zero(Sum, ParsedPolynomial).
red_sort_polynomial(Polynomial, ParsedPolynomial) :-
   as_polynomial(Polynomial, ParsedPolynomial).


%%% get_polynomial_coefficients/2
get_polynomial_coefficients(poly([]), []) :-
    !.
get_polynomial_coefficients(poly(Monomials), Coefficients) :-
    get_polynomial_coefficients(.


%%% get_polynomial_coefficients_execute/2
get_polynomial_coefficients_execute(poly([]), []) :-
    !.
get_polynomial_coefficients_execute(poly([Head | Rest]), [R | RestCoef]) :-
    get_coefficient_mono(Head, R),
    get_coefficient_polynomial_execute(poly(Rest=, RestCoef).
    

%%% get_coefficient_mono/2
get_coefficient_mono(m(C, _, _), C) :-
    !.
    
    

%%% 
    
