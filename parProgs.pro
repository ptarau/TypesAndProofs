% generator for random implicational formulas
% using give Prover to filter random formulas
% works in parallel, the first thread proving or disproving
% discard the others
ran_typed(N,PartCount,TreeCount,Prover,X:T):-
  Gen=ranImpFormulas(N,PartCount,TreeCount,G),
  Exec=call(Prover,G),
  nondet_first(G,Exec,Gen),
  ljs(X,G),
  varvars(G,T),
  !.
 
long_sprove(L,T,X):-sprove1(T,X),lsize(X,L0),L0>=L.

ran_long_proof(N,L,K,X:T):-
  Gen=ranImpFormulas(N,K,T0),
  Exec=long_sprove(L,T0,X),
  nondet_first(X:T0,Exec,Gen),
  varvars(T0,T),
  !.

% size of a canonical lambda term  
lsize(X,0):-var(X),!.
lsize(l(_,A),N):-lsize(A,N1),N is N1+1.
lsize(a(A,B),N):-lsize(A,N1),lsize(B,N2),N is 2+N1+N2.

% like select/3, but randomly
ranselect(X,Xs,Rs):-
 length(Xs,L),L>0,
 I is random(L),
 nth0(I,Xs,X0,Rs0),
 ( X=X0,Rs=Rs0
 ; ranselect(X,Rs0,Rs1),
   Rs=[X0|Rs1]
 ).
 
% sorts and trims, but in reverse standard order 
revsort(Xs,Rs):-sort(0,(@>),Xs,Rs).
 