


is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD >= 0,
	is_list(VPs).

is_polynomial(poly(Monomials)) :-
	is_list(Monomials),
	foreach(member(M, Monomials), is_monomial(M)).

get_info(Base^Exp, Base, Exp):-
	not(number(Base)),
	!.

get_info((- Base), Base, 1):-
	not(number(Base)),
	!.

get_info(Base, Base, 1):-
	not(number(Base)),
	!.

as_monomial(L, m(C, D, V1)) :-
	get_coeff(L, C),
	get_varsPowers(L, V),
	get_totDegree(V, D),
	simplifyMonomial(V, V1).

get_coeff(Head * _, C) :-
    get_coeff(Head, C),
    !.

get_coeff(- C, (-C)) :-
	number(C),
	!.

get_coeff(- C, (-1)) :-
	not(number(C)),
	!.

get_coeff(C, C) :-
    number(C),
    !.

get_coeff(C, 1) :-
    not(number(C)),
    !.


get_totDegree([v(D, _)|Vs], R):-
	get_totDegree(Vs, Tmp),
	R is Tmp + D.

get_totDegree([v(D, _)], R):-
	R is R + D.

get_totDegree([], 0).

get_varsPowers(Head * Rest, Sorted) :-
	not(number(Rest)),!,
	get_info(Rest, B, E),
	!,
	get_varsPowers(Head, Vs),
	sort(2, @=<,[v(E, B)|Vs], Sorted).

get_varsPowers(C, []) :-
	number(C),
	!.

get_varsPowers((- Head), [v(E, B)]) :-
	not(number(Head)),!,
	get_info(Head, B, E),
	!.

get_varsPowers(Head, [v(E, B)]) :-
	not(number(Head)),!,
	get_info(Head, B, E),
	!.

as_polynomial(L, poly(Monomials)):-
	get_monomials(L, Ms),
	sortMega(Ms, X),
	simplify(X, M),
	delete(M, m(0, _, _), Monomials).

simplifyMonomial([v(E1, B), v(E2, B)|Xs], Zs):-
	E3 is E1 + E2,
	simplifyMonomial([v(E3, B)|Xs], Zs).

simplifyMonomial([v(E1, B1), v(E2, B2)|Xs], [v(E1, B1)|Zs]):-
	simplifyMonomial([v(E2, B2)|Xs], Zs).

simplifyMonomial([v(E, B)|Xs], [v(E, B)|Zs]):-
	simplifyMonomial(Xs, Zs).

simplifyMonomial([],[]).

simplify([m(C1, D, V), m(C2, D, V)|Xs], [m(C3, D, V)|Zs]):-
	C3 is C1 + C2,!,
	simplify([m(C1, D, V)|Xs], [m(C1, D, V)|Zs]), !.

simplify([X|Xs], [X|Zs]):-
	simplify(Xs, Zs).

simplify([],[]).

simplify_minus([m(C1, D, V), m(C2, D, V)|Xs], [m(C3, D, V)|Zs]):-
	C3 is C1 - C2,!,
	simplify_minus([m(C1, D, V)|Xs], [m(C1, D, V)|Zs]), !.

simplify_minus([X|Xs], [X|Zs]):-
	simplify_minus(Xs, Zs).

simplify_minus([],[]).

get_monomials(Head + Rest, Sorted) :-
	!,
	as_monomial(Rest, M),
	get_monomials(Head, Ms),
	sort(2, @>= ,[M|Ms] , Sorted).

get_monomials(Head - Rest, Sorted) :-
	!,
	term_string(Rest, Tmp),
	atom_concat(-,Tmp, Tmp1),
	term_string(Tmp2, Tmp1),
	as_monomial(Tmp2, M),
	get_monomials(Head, Ms),
	sort(2, @>= ,[M|Ms] , Sorted).

get_monomials(Head, [M]) :-
	!,
	as_monomial(Head, M).

pprint_polynomial(poly([])) :-
	write('0').

pprint_polynomial(poly([X|Xs])) :-
	pprint_first(X),
	pprint_polynomial(Xs).

pprint_polynomial([X|Xs]) :-
	pprint_helper(X),
	pprint_polynomial(Xs).

pprint_polynomial([]).

pprint_first(m(C, 0, [])) :-
	!,
	write(C),
	write(' ').

pprint_first(m(C, _, V)) :-
	C==1,
	!,
	pprint_monomial(V).

pprint_first(m(C, _, V)) :-
	C==(-1),
	!,
	write('-1'),
	write(' '),
	write('*'),
	write(' '),
	pprint_monomial(V).

pprint_first(m(C, _, V)) :-
	C>=0,
	!,
	write(C),
	write(' '),
	write('*'),
	write(' '),
	pprint_monomial(V).

pprint_first(m(C, _, V)) :-
	C<0,
	!,
	write(C),
	write(' '),
	write('*'),
	write(' '),
	pprint_monomial(V).

pprint_helper(m(C, _, V)) :-
	C==1,
	write(' '),
	write('+'),
	write(' '),
	!,
	pprint_monomial(V).

pprint_helper(m(C, _, V)) :-
	C==(-1),
	write(' '),
	write('-'),
	write(' '),
	!,
	pprint_monomial(V).

pprint_helper(m(C, _, V)) :-
	C>=0,
	!,
	write(' '),
	write('+'),
	write(' '),
	write(C),
	write(' '),
	write('*'),
	write(' '),
	pprint_monomial(V).

pprint_helper(m(C, _, V)) :-
	C<0,
	!,
	write(' '),
	write(C),
	write(' '),
	write('*'),
	write(' '),
	pprint_monomial(V).

pprint_monomial([v(E, B)]) :-
    E\=1,!,
	upcase_atom(B, B1),
	write(B1),
	write('^'),
	write(E),
	write(' ').

pprint_monomial([v(_, B)]) :-
	upcase_atom(B, B1), !,
    write(B1).

pprint_monomial([v(E, B)|Vs]) :-
    E\=1, !,
	upcase_atom(B, B1),
	write(B1),
	write('^'),
	write(E),
	write(' '),
	write('*'),
	write(' '),
	pprint_monomial(Vs).

pprint_monomial([v(_, B)|Vs]) :-
	upcase_atom(B, B1), !,
    write(B1),
	write(' '),
	write('*'),
	write(' '),
	pprint_monomial(Vs).

pprint_monomial([]).

coefficients(X, Coefficients) :-
	is_polynomial(X),!,
	ordina(X, X1),
	coefficientsPoly(X1, Coefficients).

coefficients(X, Coefficients) :-
	as_polynomial(X, Y), !,
	coefficients(Y, Coefficients).

coefficientsPoly(poly([X|Xs]), [C|Coeff]) :-
	extract_coeff(X, C),
	coefficientsPoly(Xs, Coeff).

coefficientsPoly([X|Xs], [C|Coeff]) :-
	extract_coeff(X, C),
	coefficientsPoly(Xs, Coeff).

coefficientsPoly([], []).

extract_coeff(m(C, _, _), C).

variables(X, Variables):-
	is_polynomial(X), !,
	ordina(X, X1),
	variablesPoly(X1, Variables).

variables(X, Variables):-
	as_polynomial(X, Y), !,
	variablesPoly(Y, Variables).

variablesPoly(poly([m(_, _, V)|Xs]), Sorted) :-
	extract_base(V, B),
	variablesPoly(Xs, Bs),
	flatten([B|Bs], FList),
	sort(0, @<, FList , Sorted).

variablesPoly([m(_, _, V)|Xs], Sorted) :-
	extract_base(V, B),
	variablesPoly(Xs, Bs),
	flatten([B|Bs], FList),
        sort(0, @<, FList , Sorted).

variablesPoly([], []).

extract_base([v(_, B)|Xs], [B|Bs]) :-
	extract_base(Xs, Bs).

extract_base([], []).

maxdegree(X, Degree) :-
	is_polynomial(X),!,
	ordina(X, X1),
	get_degree(X1, D),
	max_list(D, Degree).

maxdegree(X, Degree) :-
	as_polynomial(X, Y), !,
	get_degree(Y, D),
	max_list(D, Degree).

mindegree(X, Degree) :-
	is_polynomial(X), !,
	ordina(X, X1),
	get_degree(X1, D),
	min_list(D, Degree).

mindegree(X, Degree) :-
	as_polynomial(X, Y), !,
	get_degree(Y, D),
	min_list(D, Degree).

get_degree(poly([m(_, D, _)|Xs]), [D|Ds]):-
	get_degree(Xs, Ds).

get_degree([m(_, D, _)|Xs], [D|Ds]):-
	get_degree(Xs, Ds).

get_degree([], []).

polyval(X, L, Value) :-
	is_polynomial(X), !,
	ordina(X, X1),
	variables(X1, V),
	polyval_helper(X1, L, Res, V),
	sumlist(Res, Value).

polyval(X, L, Value) :-
	as_polynomial(X, Y),!,
	variables(Y, V),
	polyval_helper(Y, L, Res, V),
	sumlist(Res, Value).

polyval_helper(poly([m(C, _, V)|Xs]), L, [R|Rs], Var) :-
	calcola(C, V, L, R, Var),
	polyval_helper(Xs, L, Rs, Var).

polyval_helper([m(C, _, V)|Xs], L, [R|Rs], Var) :-
	calcola(C, V, L, R, Var),
	polyval_helper(Xs, L, Rs, Var).

polyval_helper([], _, [], _).

calcola(C, [v(E, B)|Vs], L, R, V):-
	nth0(I, V, B),
	nth0(I, L, Tmp1),
	calcola(C, Vs, L, Tmp, V),
	R is  (Tmp * (Tmp1 ^ E)).

calcola(C, [], _, C, _).

monomials(X, Monomials):-
	is_polynomial(X), !,
	ordina(X, X1),
	monomialsPoly(X1, Monomials).

monomials(X, Monomials):-
	as_polynomial(X, Y), !,
	monomialsPoly(Y, Monomials).

monomialsPoly(poly([M|Ms]), [M|Rs]):-
	monomialsPoly(Ms, Rs).

monomialsPoly([M|Ms], [M|Rs]):-
	monomialsPoly(Ms, Rs).

monomialsPoly([], []).

polyplus(poly([]), P, P1) :-
	ordina(P, P1),
	!.
polyplus(P, poly([]), P1) :-
	ordina(P, P1),
	!.
polyplus(poly([]), poly([]), poly([])) :- !.

polyplus(P1, P2, R):-
	is_polynomial(P1),
	is_polynomial(P2),
	ordina(P1, P3),
	ordina(P2, P4),
	!,
	polyplusPoly(P3, P4, R).

polyplus(P1, P2, R):-
	as_polynomial(P1, X),
	as_polynomial(P2, Y),
	!,
	polyplusPoly(X, Y, R).

polyplusPoly(poly(M1), poly(M2), poly(Monomials)) :-
    append(M1, M2, R),
	sortMega(R, S),
	simplify(S, Result),
	delete(Result, m(0, _, _), Monomials).

polyminus(poly([]), P, P2) :-
	polytimes(poly([m(-1, 0, [])]), P, P1),
	ordina(P1, P2),
	!.
polyminus(P, poly([]), P1) :-
	ordina(P, P1),
	!.
polyminus(poly([]), poly([]), poly([])) :- !.

polyminus(P1, P2, R):-
	is_polynomial(P1),
	is_polynomial(P2),
	ordina(P1, P3),
	ordina(P2, P4),
	!,
	polyminusPoly(P3, P4, R).

polyminus(P1, P2, R):-
	as_polynomial(P1, X),
	as_polynomial(P2, Y),
	!,
	polyminusPoly(X, Y, R).

polyminusPoly(poly(M1), poly(M2), poly(Monomials)) :-
	append(M1, M2, R),
	sortMega(R, S),
	simplify_minus(S, Result),
	delete(Result, m(0, _, _), Monomials).

sortMega(poly(P), R1):-
	predsort(delta, P, X),
	sort(2, @=<, X, R),
	simplify(R, R1).

sortMega(P, R1):-
	predsort(delta, P, X),
	sort(2, @=<, X, R),
	simplify(R, R1).

delta(>, m( _, _, [v(NE1, B1)|_]), m(_, _, [v(NE2, B2)|_])) :-
	char_code(B1, N1),
	char_code(B2, N2),
	N1 = N2,
	NE1 > NE2.

delta(<, m(_, _, [v(NE1, B1)|_]), m(_, _, [v(NE2, B2)|_])) :-
	char_code(B1, N1),
	char_code(B2, N2),
    N1 = N2,
	NE1 =< NE2.

delta(>, m( _, _, [v(_, B1)|_]), m(_, _, [v(_, B2)|_])) :-
	char_code(B1, N1),
	char_code(B2, N2),
    N1 > N2.

delta(<, m(_, _, [v(_, B1)|_]), m(_, _, [v(_, B2)|_])) :-
	char_code(B1, X),
	char_code(B2, Y),
    X =< Y.

delta(<, m(_, _, [v(_, _)|_]), m(_, _, [])).

delta(>, m(_, _, []), m(_, _, [v(_, _)|_])).



ordina(poly([X|Xs]), poly([Z|Zs])) :-
	sortSingleMonomial(X, Z),
	ordina(Xs, Zs).

ordina([X|Xs], [Z|Zs]) :-
	sortSingleMonomial(X, Z),
	ordina(Xs, Zs).

ordina([], []).

polytimes(poly([]), _, poly([])) :- !.

polytimes(_, poly([]), poly([])) :- !.

polytimes(poly([]),poly([]), poly([])) :- !.

polytimes(M, M1, R) :-
	is_monomial(M),
	is_monomial(M1),!,
	polytimes(poly([M]), poly([M1]), R).

polytimes(M, P, R) :-
	is_monomial(M),
	is_polynomial(P), !,
	polytimes(poly([M]), P, R).

polytimes(P, M, R) :-
	is_polynomial(P),
	is_monomial(M),!,
	polytimes(P, poly([M]), R).

polytimes(Poly1, Poly2, poly(Sorted)) :-
	is_polynomial(Poly1),
	is_polynomial(Poly2), !,
	ordina(Poly1, Poly3),
	ordina(Poly2, Poly4),
	times(Poly3, Poly4, Result),
	flatten(Result, ResultF),
	sortMega(ResultF, Sorted).

polytimes(Poly1, Poly2, poly(Sorted)) :-
	as_polynomial(Poly1, X),
	as_polynomial(Poly2, Y),
	times(X, Y, Result),
	flatten(Result, ResultF),
	sortMega(ResultF, Sorted).

polytimes(Poly1, Poly2, poly(Sorted)) :-
	pippo(Poly1, X),
	as_polynomial(Poly2, Y), !,
	times(X, Y, Result),
	flatten(Result, ResultF),
	sortMega(ResultF, Sorted).

polytimes(Poly1, Poly2, poly(Sorted)) :-
	as_polynomial(Poly1, X),
	pippo(Poly2, Y), !,
	times(X, Y, Result),
	flatten(Result, ResultF),
	sortMega(ResultF, Sorted).

pippo(X, poly([X1])) :-
      as_monomial(X, X1).

sortSingleMonomial(m(A, B, V), m(A, B, S1)):-
	sort(2, @=< , V, S),
	simplifyMonomial(S, S1).

times([X|Xs], poly(Ys), [R|Result]) :-
	polytimes_helper(X, Ys, R),
	times(Xs, Ys, Result).

times(poly([X|Xs]), poly(Ys), [R|Result]) :-
	polytimes_helper(X, Ys, R),
	times(Xs, Ys, Result).

times([X|Xs], Ys, [R|Result]):-
	polytimes_helper(X, Ys, R),
	times(Xs, Ys, Result).

times([], _, []).

polytimes_helper(M1, [P|Ps], [R|Rs]) :-
	m_monomials(M1, P, X),
	sortSingleMonomial(X, R),
	polytimes_helper(M1, Ps, Rs).

polytimes_helper(_, [], []).

m_monomials(m(C1, 0, []),
	    m(C2, 0, []),
	    m(C3, 0, [])) :-
	C3 is C1 * C2.

m_monomials(m(C1, 0, []),
	    m(C2, _, [v(E2, B)|Ys]),
	    m(C3, D, [v(E2, B)|Zs])) :-
	C3 is C1 * C2,
	m_monomials([],Ys,Zs),
	get_totDegree([v(E2, B)|Zs], D).

m_monomials(m(C1, _, [v(E1, B)|Xs]),
	    m(C2, 0, []),
	    m(C3, D, [v(E1, B)|Zs])) :-
	C3 is C1 * C2,
	m_monomials(Xs, [], Zs),
	get_totDegree([v(E1, B)|Zs], D).

m_monomials(m(C1, _, [v(E1, B)|Xs]),
	    m(C2, _, [v(E2, B)|Ys]),
	    m(C3, D, [v(E3, B)|Zs])) :-
	C3 is C1 * C2,
	E3 is E1 + E2,
	m_monomials(Xs, Ys, Zs),
	get_totDegree([v(E3, B)|Zs], D).

m_monomials(m(C1, _, [v(E1, B1)|Xs]),
	    m(C2, _, [v(E2, B2)|Ys]),
	    m(C3, D, [v(E1,B1),v(E2,B2)|Zs])) :-
	C3 is C1 * C2,
	m_monomials(Xs, Ys, Zs),
	get_totDegree([v(E1,B1), v(E2,B2)|Zs], D).

m_monomials([v(E1, B)|Xs], [v(E2, B)|Ys], [v(E3, B)|Zs]) :-
	E3 is E1 + E2,
	m_monomials(Xs, Ys, Zs).

m_monomials([v(E1, B1)|Xs], [v(E2, B2)|Ys], [v(E1, B1),v(E2, B2)|Zs]) :-
	m_monomials(Xs, Ys, Zs).

m_monomials([], [v(E2, B2)|Ys], [v(E2, B2)|Zs]) :-
	m_monomials([], Ys, Zs).

m_monomials([v(E1, B1)|Xs], [], [v(E1, B1)|Zs]) :-
	m_monomials(Xs, [], Zs).

m_monomials([], [], []).
