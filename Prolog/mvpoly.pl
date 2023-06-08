%%%% -*- Mode: Prolog -*-
%%%% <matricola> <cognome> <nome>
%%%% <collaboratori>


%%% is_monomial/1 

is_monomial(m(_C, TD, VPs)) :-
    integer(TD),
    TD >= 0,
    is_list(VPs).

%%% is_varpower/1

is_varpower(v(Power, VarSymbol)) :-
    integer(Power),
    Power >= 0,
    atom(VarSymbol).


%%% is_polynomial/1
%%% is_polynomials(poly[Monomials])
%%% true when Monomials is an list composed of monomails

is_polynomial(poly(Monomials)) :-
    is_list(Monomials),
    foreach(member(M, Monomials), is_monomial(M)).


%%% coefficients/2
%%% coefficients(Poly, Coefficients)
%%% true whene Coefficients is a list composed of
%%% the coefficients of polinomial

coefficients(poly([]), []).

coefficients(Poly, Coefficients) :-
    is_polynomial(poly(Poly)).
    
    
