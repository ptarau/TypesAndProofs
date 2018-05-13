% random typed NF with fixed seed
genRanTyped:-genRanTyped(1003).


% generates random typed NFs with given seed
% Seed=random generates them pseudo-randomly

genRanTyped(Seed):-
  retractall(sure(_,_)),
  parRanTypedTNF(Seed,50,70,1000,X:T,_),
  ppp(X),ppp(T),nl,
  assertz(sure(X,T)),
  fail
; true.

maybe_type(T):-
  ranImpFormulas(15,5,5,T).
	

  % show generation of random NFs of size 20
show_ranNF:-ranNF(20,X:T,_),
   numbervars(X,10,_),
   writeln(X),writeln(T),fail.
