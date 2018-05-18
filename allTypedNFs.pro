% generate all simply typed normal forms of given size

nfTypes(N,T):-tnf(N,_:T),natvars(T).

% typable normal form of size N
tnf(N,X:T):-n2s(N,S),tnf(X,T,[],S,0,Gs,true),Gs.

tnfs(N,X,T):-tnf(N,X,T,Gs,true),Gs.

tnf(N,X,T,Gs,Gs0):-
  n2s(N,S),
  tnf(X,T,[],S,_,Gs,Gs0).

tnf(X,P,[Q|Ps],N,N)-->add_hypo(X,P,[Q|Ps]).
tnf(l(X,A),(P->Q),Ps,s(N1),N2)--> 
  % introduce P, for proving P->Q
  tnf(A,Q,[X:P|Ps],N1,N2).  
tnf(a(A,B),Q,Ps,s(s(N1)),N3)--> 
  % try Modus Ponens for Q, by proving P->Q and P
  tnf0(A,(P->Q),Ps,N1,N2),
  tnf(B,P,Ps,N2,N3).

tnf0(X,P,[Q|Ps],N,N)--> add_hypo(X,P,[Q|Ps]).
tnf0(a(A,B),Q,Ps,s(s(N1)),N3)--> 
  % try Modus Ponens for Q, by proving P->Q and P
  tnf0(A,(P->Q),Ps,N1,N2),
  tnf(B,P,Ps,N2,N3).

add_hypo(X,P,Ps,(hypo(X,P,Ps),Gs),Gs).

hypo(X,P,Ps):-member(X:Q,Ps),unify_with_occurs_check(P,Q).


% computes type of an expression X
type_of(X,T):-type_of(X,T,[]).

type_of(X,T0,Vs):-var(X),!,
  member(X0:T,Vs),X==X0,
  unify_with_occurs_check(T0,T).
type_of(l(X,A),(S->T),Vs):-
  type_of(A,T,[X:S|Vs]).
type_of(a(A,B),T,Vs):-
  type_of(A,(S->T),Vs),
  type_of(B,S,Vs).

  
  /*
  ?- findall(S,(between(0,16,N),sols(tnf(N,_),S)),Xs).
[0,1,2,3,7,17,43,129,389,1245,4274,14991,55289,210743,826136,3354509,13948176]
Xs = [0,1,2,3,7,17,43,129,389,1245,4274,14991,55289,210743,826136,3354509,13948176].

  
  */