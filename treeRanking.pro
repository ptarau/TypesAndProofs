% bijection between implicational formula tree skeletons and natural numbers

% successor
s(0,0->0).
s(X->0,X->(0->0)):-!.
s(X->Xs,Z):-parity(X->Xs,P),s1(P,X,Xs,Z).

s1(0,0,X->Xs,SX->Xs):-s(X,SX).
s1(0,X->Ys,Xs,0->(PX->Xs)):-p(X->Ys,PX).
s1(1,X,0->(Y->Xs),X->(SY->Xs)):-s(Y,SY).
s1(1,X,Y->Xs,X->(0->(PY->Xs))):-p(Y,PY).

% predecessor
p(0->0,0).
p(X->(0->0),X->0):-!.
p(X->Xs,Z):-parity(X->Xs,P),p1(P,X,Xs,Z).

p1(0,X,0->(Y->Xs),X->(SY->Xs)):-s(Y,SY).
p1(0,X,(Y->Ys)->Xs,X->(0->(PY->Xs))):-p(Y->Ys,PY).
p1(1,0,X->Xs,SX->Xs):-s(X,SX).
p1(1,X->Ys,Xs, 0->(PX->Xs)):-p(X->Ys,PX).

% parity, starting with even, then flipping with 2nd argument
parity(0,0).
parity(_->0,1).
parity(_->(X->Xs),P1):-parity(X->Xs,P0),P1 is 1-P0.

% equivalent of fuses two tree under the same root, but done with numbers
cons(I,J,C) :- I>=0,J>=0,
  D is mod(J+1,2),
  C is 2^(I+1)*(J+D)-D.

% separate two branches of a tree equivalent on the N side
decons(K,I1,J1):-K>0,B is mod(K,2),KB is K+B,
  dyadicVal(KB,I,J),
  I1 is max(0,I-1),J1 is J-B.

  % counts how many time a number can be divided by 2, exactly
dyadicVal(KB,I,J):-I is lsb(KB),J is KB // (2^I).

% bijection from T (seet of trees) to N
n(0,0).
n((A->B),K):-n(A,I),n(B,J),cons(I,J,K).

% bijection from N to T
t(0,0).
t(K,(A->B)):-K>0,decons(K,I,J),t(I,A),t(J,B).

% variant of t/2 with fresh variables at leaves

t_(K,T,Vs):-t_(K,T,Vs,[]).

t_(0,V)-->[V].
t_(K,(A->B))-->{K>0,decons(K,I,J)},t_(I,A),t_(J,B).






