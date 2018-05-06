% simple evaluator / truth table generator
% for classic implicational formulas

classEval(G):-
  varvars(G,F),
  term_variables(F,Vs),
  evalT(F,R),
  ppp(Vs:R),
  fail.

taut(X):- \+ eval(X,0).

taut0(G):-must_be(ground,G),
  varvars(G,X),
  term_variables(X,Vs),
  ppp(X:Vs),
  evalT(X,R),
  ppp(X:Vs=R),fail;true.

eval(G,R):-varvars(G,T),evalT(T,R).

evalT(X,X):-var(X),!,bit(X).
evalT(false,R):-!,R=0.
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



tprove(F):-toImp(F,I),dneg(I,NNI),kprove(NNI).

toImp(X,R):-atomic(X),!,R=X.
toImp((X->Y),(A->B)):-toImp(X,A),toImp(Y,B).

toImp(~X,(A->false)):-toImp(X,A).
toImp(X*Y,  ((A -> (B -> false))->false)):-
  toImp(X,A),
  toImp(Y,B).
toImp(X+Y,  (A->false)->B) :-
  toImp(X,A),
  toImp(Y,B).
toImp(X=Y,R):-
  toImp((X->Y)*(Y->X),R).
toImp(X^Y, R):-
  toImp(~(X->Y) + ~(Y->X), R).

/*
  
% TODO

% shannon expansion - for (todo) use in fast classical
% tautology checking
shannon(V,T,T0,T1):-shannon0(V,T,T0),shannon1(V,T,T1).
  
  
shannon0(V,(V->_),true):-!.
shannon0(V,(X ->V),(NewX->false)):-!,shannon0(V,X,NewX).
shannon0(V,W,W):-atomic(W),W\=V,!.
shanoon0(V,(X->Y),(A->B)):-shannon0(V,X,A),shannon0(V,Y,B).

shannon1(V,(_ -> V),true):-!.
shannon1(V,(V -> X),NewX):-!,shannon1(V,X,NewX).
shannon1(V,W,W):-atomic(W),W\=V,!.
shannon1(V,(X->Y),(A->B)):-shannon1(V,X,A),shannon1(V,Y,B).
*/

