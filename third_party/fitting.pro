% sequent calculus-based classical propositional tautology prover
% specialized to the implcational fragment

% derived from Melvin Fitting's prover
% specialized to implication and negation only

/*
the heart of the matter:
thm(Lambda, Gamma) :-
the sequent Lambda --> Gamma is provable.
*/

thm(Lambda, [~ (~ (X)) | Gamma]) :-
  !,
  thm(Lambda, [X | Gamma]).
thm(Lambda, [(X -> Y) | Gamma]) :-
  !,
  thm(Lambda, [~X, Y | Gamma]).
thm(Lambda, [~(X -> Y) | Gamma]) :-
  !,
  thm(Lambda, [X | Gamma]), 
  !,
  thm(Lambda, [~Y | Gamma]).
thm([L1|Lambda], [L2|_]) :-
  ( L1 = L2, 
    ! 
  ; 
   thm(Lambda, [L2])
  ).
thm(Lambda, [~ L | Gamma]) :-
  thm([L | Lambda], Gamma), 
  !.
thm(Lambda, [L | Gamma]) :-
  thm([~ L | Lambda], Gamma), 
  !.

tautology(X) :- thm([], [X]).


ftest1:-
  tautology(0 -> 1 ->0),
  tautology( ~ ~ 0 -> 0),
  tautology((0->1->2)->(0->1)->0->2).
  