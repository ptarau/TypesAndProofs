% Melvin Fittings sequent calculus-based classical propositional tautology prover

:-op(100, fy, neg).
:-op(110, yfx, and).
:-op(120, yfx, or).
:-op(130, xfy, imp).
:-op(130, xfy, iff).

type(X and Y, conj, X, Y).
type(neg(X and Y), disj, neg X, neg Y).
type(X or Y, disj, X, Y).
type(neg(X or Y), conj, neg X, neg Y).
type(X imp Y, disj, neg X, Y).
type(neg(X imp Y), conj, X, neg Y).
type(X iff Y, disj, X and Y, neg X and neg Y).
type(neg(X iff Y), disj, X and neg Y, neg X and Y).
type(neg (neg (X)), doub, X, _).

/*
Now the heart of the matter.
thm(Lambda, Gamma) :-
the sequent Lambda --> Gamma is provable.
*/

thm(Lambda, [Doubleneg | Gamma]) :-
type(Doubleneg, doub, X, _), !,
thm(Lambda, [X | Gamma]).
thm(Lambda, [Beta | Gamma]) :-
type(Beta, disj, Beta1, Beta2), !,
thm(Lambda, [Beta1, Beta2 | Gamma]).
thm(Lambda, [Alpha | Gamma]) :-
type(Alpha, conj, Alpha1, Alpha2), !,
thm(Lambda, [Alpha1 | Gamma]), !,
thm(Lambda, [Alpha2 | Gamma]).
thm([L1|Lambda], [L2|_]) :-
(L1 = L2, ! ; thm(Lambda, [L2])).
thm(Lambda, [neg L | Gamma]) :-
thm([L | Lambda], Gamma), !.
thm(Lambda, [L | Gamma]) :-
thm([neg L | Lambda], Gamma), !.

tautology(X) :- thm([], [X]).


go:-
  tautology(a imp b).
  