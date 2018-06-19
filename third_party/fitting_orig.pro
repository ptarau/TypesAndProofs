% Melvin Fittings sequent calculus-based classical propositional tautology prover
% with operator names adjusted to match other provers

%:-op(100, fy,  ~ ).
%:-op(110, yfx,  & ).
%:-op(120, yfx,  v  ).
%:-op(130, xfy, ->).
%:-op(130, xfy, <->).

type(X  &  Y, conj, X, Y).
type( ~ (X  &  Y), disj,  ~  X,  ~  Y).
type(X  v   Y, disj, X, Y).
type( ~ (X  v   Y), conj,  ~  X,  ~  Y).
type(X -> Y, disj,  ~  X, Y).
type( ~ (X -> Y), conj, X,  ~  Y).
type(X <-> Y, disj, X  &  Y,  ~  X  &   ~  Y).
type( ~ (X <-> Y), disj, X  &   ~  Y,  ~  X  &  Y).
type( ~  ( ~  (X)), doub, X, _).

/*
Now the heart of the matter.
thm(Lambda, Gamma) :-
the sequent Lambda --> Gamma is provable.
*/

thm(Lambda, [Doubleneg  | Gamma]) :-
type(Doubleneg , doub, X, _), !,
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
thm(Lambda, [ ~  L | Gamma]) :-
thm([L | Lambda], Gamma), !.
thm(Lambda, [L | Gamma]) :-
thm([ ~  L | Lambda], Gamma), !.

tautology(X) :- thm([], [X]).


go:-
  tautology(a -> b).
  