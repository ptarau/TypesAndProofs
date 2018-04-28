classEval(G):-
  varvars(G,F),
  term_variables(F,Vs),
  evalT(F,R),
  ppp(Vs:R),
  fail.

taut(X):- \+eval(X,0).

taut0(G):-must_be(ground,G),
  varvars(G,X),
  term_variables(X,Vs),
  ppp(X:Vs),
  evalT(X,R),
  ppp(X:Vs=R),fail;true.

eval(G,R):-varvars(G,T),evalT(T,R).

evalT(X,X):-var(X),!,bit(X).
evalT(false,0).
evalT(X,R):-integer(X),!,R=X.
evalT((A->B),R):-
  evalT(A,X),
  evalT(B,Y),
  impl(X,Y,R).

impl(0,0,1).
impl(0,1,1).
impl(1,0,0).
impl(1,1,1).

bit(0).
bit(1).

