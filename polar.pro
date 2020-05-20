%  utilities

/*
is_nf(E):-is_neut(E),!.
is_nf(l(_,E)):-is_nf(E).

is_neut(X):-var(X),!.
is_neut(a(E,F)):-is_neut(E),is_nf(F).

lsize(A,R):-var(A),!,R=0.
lsize(A,R):-atomic(A),!,R=0.
lsize(l(_,B),R):-lsize(B,R2),R is 1+R2.
lsize(a(A,B),R):-lsize(A,R1),lsize(B,R2),R is 1+R1+R2.

pol(A,X):-pol(0,A,X).

pol(P,X,R):-var(X),!,R=P:X.
pol(P,A '-o' B,(X '-o' Y)):-N is 1-P,pol(N,A,X),pol(P,B,Y).
*/

% generate polarized
polarized_tree(L,Tree):-L>=0,
  K is L+1,
  N is 2*L+1, 
  numlist(0,L,Ps),
  length(Qs,K), 
  polarized_tree(0,Tree,Ps,[],Qs,[],N,0),
  perm_of(Ps,Qs).
  
polarized_tree(P,V,Xs1,Xs2,Ys1,Ys2)-->{dispatch(P,V,Xs1,Xs2,Ys1,Ys2)}.
polarized_tree(P,(A  '-o' B),Xs1,Xs3,Ys1,Ys3)-->pred,{Q is 1-P},
  polarized_tree(Q,A,Xs1,Xs2,Ys1,Ys2),
  polarized_tree(P,B,Xs2,Xs3,Ys2,Ys3).
  
perm_of([],[]).
perm_of([X|Xs],Zs):-
  perm_of(Xs,Ys), % handling the (shorter) tail of the list
  ins(X,Ys,Zs). % inductive step: insert X into Ys

ins(X,Xs,[X|Xs]).
ins(X,[Y|Xs], [Y|Ys]):-ins(X,Xs,Ys).



  

  
  
% checks if a proof term in normal form can be associated to it  
is_linear_typed_normal_form(N,E,T):-succ(N,N1),
  is_linear_typed_normal_form(E,T,N,0,N1,0,[]).

is_linear_typed_normal_form(l(X,E),(S '-o' T),A1,A2,L1,L3,Vs):-pred(L1,L2),
  is_linear_typed_normal_form(E,T,A1,A2,L2,L3,[V:S|Vs]),
  check_binding(V,X),
  !.
is_linear_typed_normal_form(E,T,A1,A2,L1,L3,Vs):-
  is_linear_neutral_term(E,T,A1,A2,L1,L3,Vs).

is_linear_neutral_term(X,T,A,A,L,L,Vs):-
  member(V:TT,Vs),
  bind_once(V,X),T=TT,
  !.
is_linear_neutral_term(a(E,F),T,A1,A4,L1,L3,Vs):- pred(A1,A2),
  is_linear_neutral_term(E,(S '-o' T),A2,A3,L1,L2,Vs),
  is_linear_typed_normal_form(F,S,A3,A4,L2,L3,Vs).
  
tsize(A,R):-var(A),!,R=0.
tsize(A,R):-atomic(A),!,R=0.
tsize((A'-o'B),R):-tsize(A,R1),tsize(B,R2),R is 1+R1+R2.

%%%%%%%%%% begin prover
  
% check if polarized
is_polarized_tree(Tree,N):-
  is_polarized_tree(0,Tree,Ps,[],Qs,[]),
  in_bijection(Ps,Qs,N).
  
is_polarized_tree(P,V,Xs1,Xs2,Ys1,Ys2):-atomic(V),!,
  dispatch(P,V,Xs1,Xs2,Ys1,Ys2).
is_polarized_tree(P,(A  '-o' B),Xs1,Xs3,Ys1,Ys3):-Q is 1-P,
  is_polarized_tree(Q,A,Xs1,Xs2,Ys1,Ys2),
  is_polarized_tree(P,B,Xs2,Xs3,Ys2,Ys3).

dispatch(0,X,[X|Xs],Xs,Ys,Ys).
dispatch(1,Y,Xs,Xs,[Y|Ys],Ys).

% each variable has a counterpart of opposite polarity  
in_bijection(Ps,Qs, N):-
  length(Ps,K),
  length(Qs,K),
  sort(Ps,Xs),
  sort(Qs,Xs),
  length(Xs,K),
  N is K-1.
  
% implicational linear logic prover T in, X out  
prove_polarized(X,T):-
  is_polarized_tree(T,N),
  linear_typed_normal_form(N,X,T).
 
%%%%%%%%%% end prover  
  
 
% generators/testers  

% test prover - should match linear_typed_normal_form
pol_taut(N,X,T):-
  gen_formula2(N,T),
  prove_polarized(X,T).
  
pol_gen_taut(N,X,T):-
  polarized_tree(N,T),
  is_linear_typed_normal_form(N,X,T).
  

pol_gen(N,T):-
  gen_formula2(N,T),
  is_polarized_tree(T,_).
    
  
  

/*

LinearTermsAndType(7)=[1,3,26,367,7142,176766,5304356,186954535]

PolarizedLinearTautologies(5)=[1,1,11,119,1679,30239]
*/

gpp(N):-counts_for2(N,polarized_tree,Ks),ppp('PolarizedTrees'(N)=Ks).

gpg(N):-counts_for3(N,pol_gen_taut,Ks),ppp('tests'(N)=Ks). % good

gpf(N):-counts_for3(N,pol_taut,Ks),ppp('tests'(N)=Ks). % good

