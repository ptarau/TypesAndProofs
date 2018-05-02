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

tautology(X0) :- false2neg(X0,X),thm([], [X]).



false2neg(false,R):-!,R = ~ (0->0).
false2neg(X,R):-atomic(X),!,R=X.
false2neg((X->false),R):-!,false2neg(X,A),R= ~A.
false2neg((X->Y),(A->B)):-false2neg(X,A),false2neg(Y,B).

ftest1:-
  tautology(0 -> 1 ->0),
  tautology( ~ ~ 0 -> 0),
  tautology((0->1->2)->(0->1)->0->2).
  