% random typable normal form, using Boltzmann sampler
% combined with fast type inference algorithm

ranTNF(N,X:T,TSize):-ranTNF(random,N,1,X:T,_Size),tsize(T,TSize).

ranSeededTNF(Seed,N,K,T):-ranTNF(Seed,N,K,_X:T,_Size).

% API - replicable
ranTNF(Seed,N,K,X:T,Size):-
  set_random(seed(Seed)),
  MaxSteps=1000000,
  Max is truncate(N*(11/10)),
  Min is truncate(N*(9/10)),  
  between(1,K,_),
  ranTypableNF0(Max,Min,MaxSteps,X,T,Size,_Steps),
  natvars(T).
  

boltzmann_nf_lambda(R):-R<0.3333158264186935. % an l/1, otherwise neutral
boltzmann_nf_index(R):-R<0.5062759837493023.  % neutral: index, not a/2
boltzmann_nf_leaf(R):-R<0.6666841735813065.   % neutral: 0, otherwise s/1

ranTypableNF0(Max,Min,MaxSteps,X,T,Size,Steps):-
  tryRanTypableNF(Max,Min,0,MaxSteps,X,T,Size,Steps),
  !.

% API element  
tryRanTypableNF(Max,Min,TSize0,MaxSteps,X,T,Size,Steps):-
  tryRanTypableDbNF(Max,Min,TSize0,MaxSteps,X0,T,Size,Steps),
  %assert(is_nf(X0)),
  db2std(X0,X).
  
tryRanTypableDbNF(Max,Min,TSize0,MaxSteps,X0,T,Size,Steps):-
  between(1,MaxSteps,Steps),
    random(R),
    ranTypableNF(Max,R,X0,T,[],0,Size0),
  Size0>=Min,
  tsize(T,TSize),TSize>=TSize0,
  Size is Size0+1.
  
db2std(D,X):-db2std(D,[],X).

db2std(0,[V|_],V).
db2std(s(I),[_|Vs],V):-db2std(I,Vs,V).
db2std(l(A),Vs,l(V,B)):-db2std(A,[V|Vs],B).
db2std(a(A,B),Vs,a(X,Y)):-db2std(A,Vs,X),db2std(B,Vs,Y).

repSize(0,0).
repSize(s(_),0).
repSize(l(A),S):-repSize(A,P),S is P+1.
repSize(a(A,B),S):-repSize(A,S1),repSize(B,S2),S is S1+S2+2.


ranTypableNF(Max,R,l(A),(X->Xs),Vs,N1,N3):-
  boltzmann_nf_lambda(R),!, %lambda
  next(Max,NewR,N1,N2),
  ranTypableNF(Max,NewR,A,Xs,[X|Vs],N2,N3).  

ranTypableNF(Max,R,X,V,Vs,N1,N2):-boltzmann_nf_index(R),!,
  random(NewR),
  pickIndexNF(Max,NewR,X,Vs,V,N1,N2). % an index
ranTypableNF(Max,_R,a(A,B),Xs,Vs,N1,N5):- % an application
  next(Max,R1,N1,N2),
  ranTypableNF(Max,R1,A,(X->Xs),Vs,N2,N3),
  next(Max,R2,N3,N4),
  ranTypableNF(Max,R2,B,X,Vs,N4,N5).

pickIndexNF(_,R,0,[V|_],V0,N,N):-boltzmann_nf_leaf(R),!, % zero
  unify_with_occurs_check(V0,V).
pickIndexNF(Max,_,s(X),[_|Vs],V,N1,N3):- % successor
  next(Max,NewR,N1,N2),
  pickIndexNF(Max,NewR,X,Vs,V,N2,N3).	

next(Max,R,N1,N2):-N1<Max,N2 is N1+1,random(R).  

tsize(A,R):-var(A),!,R=0.
tsize(A,R):-atomic(A),!,R=0.
tsize(A->B,S):-tsize(A,S1),tsize(B,S2),S is 1+S1+S2.


% optional transformer from de Bruij to canonical forms
nb2l(A,T):-nb2l(A,T,_Vs).

nat1(0).
nat1(s(_)).

% from de Bruijn to canonical lambda term
nb2l(I,V,Vs):-nat1(I),s2n(I,N),nth0(N,Vs,V).
nb2l(a(A,B),a(X,Y),Vs):-nb2l(A,X,Vs),nb2l(B,Y,Vs).
nb2l(l(A),l(V,Y),Vs):-nb2l(A,Y,[V|Vs]).

is_nf(0).
is_nf(s(_)).
is_nf(l(A)):-is_nf(A).
is_nf(a(l(_),_)):-!,fail.
is_nf(a(A,B)):-is_nf(A),is_nf(B).
