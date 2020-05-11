% could generate linear tautologies if prover would always 
% proof terms in normal form
% !!! not working yet as proof terms would need beta reduction before tested


% I thought that improvment is possible by marking term skeletons with polarities and then
% decorating each polarity with a partition of the same N variables for a tree of size 2N+1

gen_pol_taut(N,T,ProofTerm):-
  gen_polarized(N,T),
  prove_lin(T,ProofTerm),
  is_nf(ProofTerm).

is_nf(E):-is_neut(E),!.
is_nf(l(_,E)):-is_nf(E).

is_neut(X):-var(X),!.
is_neut(a(E,F)):-is_neut(E),is_nf(F).

gen_polarized(N,Tree):-
  polarized_tree(N,Tree,Ps,Qs),
  perm_of(Ps,Qs).

  
polarized_tree(N,Tree,Ps,Qs):-N>0,
  K is (N+1)//2,L is K-1,
  length(Ps,K),
  length(Qs,K),
  numlist(0,L,Ps),
  polarized_tree(0,Tree,Ps,[],Qs,[],N,0).
  

polarized_tree(P,V,Xs1,Xs2,Ys1,Ys2)-->dispatch(P,V,Xs1,Xs2,Ys1,Ys2).
polarized_tree(P,(A  -@ B),Xs1,Xs3,Ys1,Ys3)-->pred,{Q is 1-P},
  polarized_tree(Q,A,Xs1,Xs2,Ys1,Ys2),
  polarized_tree(P,B,Xs2,Xs3,Ys2,Ys3).
  
dispatch(0,X,[X|Xs],Xs,Ys,Ys,N,N).
dispatch(1,Y,Xs,Xs,[Y|Ys],Ys,N,N).

perm_of([],[]).
perm_of([X|Xs],Zs):-
  perm_of(Xs,Ys), % handling the (shorter) tail of the list
  ins(X,Ys,Zs). % inductive step: insert X into Ys

ins(X,Xs,[X|Xs]).
ins(X,[Y|Xs], [Y|Ys]):-ins(X,Xs,Ys).

% gp(N):-counts_for3(N,gen_pol_taut,Ks),ppp('LinearTautologies'(N)=Ks).