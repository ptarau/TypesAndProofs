% could generate linear tautologies if prover would always 
% proof terms in normal form
% !!! not working yet as proof terms would need beta reduction before tested


% I thought that improvment is possible by marking term skeletons with polarities and then
% decorating each polarity with a partition of the same N variables for a tree of size 2N+1

gen_pol_taut(N,T,ProofTerm):-
  M is 2*N+1,
  polarized_tree(N,T),
  prove_lin(T,ProofTerm),
  is_nf(ProofTerm),
  lsize(ProofTerm,M),
  type_of(ProofTerm,T),
  true.

is_nf(E):-is_neut(E),!.
is_nf(l(_,E)):-is_nf(E).

is_neut(X):-var(X),!.
is_neut(a(E,F)):-is_neut(E),is_nf(F).

lsize(A,R):-var(A),!,R=0.
lsize(A,R):-atomic(A),!,R=0.
lsize(l(_,B),R):-lsize(B,R2),R is 1+R2.
lsize(a(A,B),R):-lsize(A,R1),lsize(B,R2),R is 1+R1+R2.

tsize(A,R):-var(A),!,R=0.
tsize(A,R):-atomic(A),!,R=0.
tsize((A'-o'B),R):-tsize(A,R1),tsize(B,R2),R is 1+R1+R2.


% computes type of an expression X
type_of(X,T):-type_of(X,T,[]).

type_of(X,T0,Vs):-var(X),!,
  member(X0:T,Vs),X==X0,T0=T.
type_of(l(X,A),(S '-o' T),Vs):-
  type_of(A,T,[X:S|Vs]).
type_of(a(A,B),T,Vs):-
  type_of(A,(S '-o' T),Vs),
  type_of(B,S,Vs).
  

polarized_tree(N,Tree):-
  polarized_tree(N,Tree,_Ps,_Qs).
 
polarized_tree(L,Tree,Ps,Qs):-L>=0,
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
  
dispatch(0,X,[X|Xs],Xs,Ys,Ys).
dispatch(1,Y,Xs,Xs,[Y|Ys],Ys).

perm_of([],[]).
perm_of([X|Xs],Zs):-
  perm_of(Xs,Ys), % handling the (shorter) tail of the list
  ins(X,Ys,Zs). % inductive step: insert X into Ys

ins(X,Xs,[X|Xs]).
ins(X,[Y|Xs], [Y|Ys]):-ins(X,Xs,Ys).

pol(A,X):-pol(0,A,X).

pol(P,X,R):-var(X),!,R=P:X.
pol(P,A '-o' B,(X '-o' Y)):-N is 1-P,pol(N,A,X),pol(P,B,Y).


check_lin(N,X,T):-
  is_linear_typed_normal_form(N,X,T),
  %is_nf(X),
  %type_of(X,T),
  M is 2*N+1,lsize(X,M),
  term_variables(T,Vs),
  length(Vs,L),K is L-1,
  numlist(0,K,Vs),
  %\+once(gen_pol_taut(N,T,_)),
  %\+once(polarized_tree(M,T)),
  once(prove_ipc(T,XX)),
  ppt(xxx=X),
  ppt(xxxxx=XX),
  type_of(XX,TT),
  ppp(ttt=T),
  ppp(tttttt=TT),
  (is_linear(XX)->ppp(linear);ppp(not_lin),abort),
  
  ppp('********************'),nl.

gpp(N):-counts_for2(N,polarized_tree,Ks),ppp('PolarizedTrees'(N)=Ks).
gpt(N):-counts_for3(N,gen_pol_taut,Ks),ppp('PolarizedLinearTautologies'(N)=Ks).
gpl(N):-counts_for3(N,check_lin,Ks),ppp('tests'(N)=Ks).

gpg(N):-counts_for3(N,pol_gen_taut,Ks),ppp('tests'(N)=Ks).


gplt(N):-counts_for2(N,pol_gen,Ks),ppp('tests'(N)=Ks).

gpf(N):-counts_for3(N,pol_taut,Ks),ppp('tests'(N)=Ks).


bug:-gpl(2).

/*

LinearTermsAndType(7)=[1,3,26,367,7142,176766,5304356,186954535]

PolarizedLinearTautologies(5)=[1,1,11,119,1679,30239]
*/


is_polarized_tree(Tree):-
  is_polarized_tree(0,Tree,Ps,[],Qs,[]),
  length(Ps,K),
  length(Qs,K),
  sort(Ps,Xs),
  sort(Qs,Ys),
  Xs=Ys,
  length(Xs,K).
  

is_polarized_tree(P,V,Xs1,Xs2,Ys1,Ys2):-atomic(V),!,
  dispatch(P,V,Xs1,Xs2,Ys1,Ys2).
is_polarized_tree(P,(A  '-o' B),Xs1,Xs3,Ys1,Ys3):-Q is 1-P,
  is_polarized_tree(Q,A,Xs1,Xs2,Ys1,Ys2),
  is_polarized_tree(P,B,Xs2,Xs3,Ys2,Ys3).
  
  
is_linear_typed_normal_form(E,T):-
  is_linear_typed_normal_form(E,T,[]).

is_linear_typed_normal_form(l(X,E),(S '-o' T),Vs):-
  is_linear_typed_normal_form(E,T,[V:S|Vs]),
  check_binding(V,X),
  !.
is_linear_typed_normal_form(E,T,Vs):-
  is_linear_neutral_term(E,T,Vs).

is_linear_neutral_term(X,T,Vs):-
  member(V:TT,Vs),
  bind_once(V,X),T=TT,
  !.
is_linear_neutral_term(a(E,F),T,Vs):-
  is_linear_neutral_term(E,(S '-o' T),Vs),
  is_linear_typed_normal_form(F,S,Vs).
  
prove_polarized(X,T):-
  is_polarized_tree(T),
  is_linear_typed_normal_form(X,T).
  
pol_gen_taut(N,X,T):-
  polarized_tree(N,T),
  is_linear_typed_normal_form(X,T).
  
pol_gen(N,T):-
  gen_formula2(N,T),
  is_polarized_tree(T).
  
pol_taut(N,X,T):-
  gen_formula2(N,T),
  prove_polarized(X,T).
  
  
  