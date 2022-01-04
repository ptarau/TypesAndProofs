% provers using embedded Horn Clauses or customized
% for also handling negation and classic proofs
% via Glivenko's translation from classical to intuitionistic
% propositional calculus


gprove(T0):-dneg_expand(T0,T),ljk(T).

dneg(X,((X->false)->false)).

dneg_expand(T0,T):-expand_neg(T0,T1),dneg(T1,T).

kprove(T0):-expand_neg(T0,T),ljk(T).
 
ljk(T):-ljk(T,[]),!.

ljk(_,Vs):-memberchk(false,Vs),!.
ljk(A,Vs):-memberchk(A,Vs),!.
ljk((A->B),Vs):-!,ljk(B,[A|Vs]). 
ljk(G,Vs1):-
  select((A->B),Vs1,Vs2),
  ljk_imp(A,B,Vs2),
  !,
  ljk(G,[B|Vs2]).

ljk_imp((C->D),B,Vs):-!,ljk((C->D),[(D->B)|Vs]).
ljk_imp(A,_,Vs):-memberchk(A,Vs).   


expand_neg(~A,R):-!,expand_neg(A,B),R=(B->false).
expand_neg((A->B),(X->Y)):-!,expand_neg(A,X),expand_neg(B,Y).
%expand_neg(f,false):-!.
expand_neg(A,R):-R=A.

% classicall logic propositional prover
% using Glivenko's double negation translation

% Glivenko's translation of a classical tautology
% is an intuitionistic tautology
cgprove(T0):-dneg(T0,T),cprove(T).

% handles also the atom "false" as a special case
% supports also negation seen as A->false
cprove(T0):-
 expand_neg(T0,T),
 ljc(T,[]),
 !.

ljc(_,Vs):-memberchk(false,Vs),!.
ljc(A,Vs):-memberchk(A,Vs),!.
ljc((A->B),Vs):-!,ljc(B,[A|Vs]). 
ljc(G,Vs1):- % atomic(G),
  select((A->B),Vs1,Vs2),
  ljc_imp(A,B,Vs2),
  !,
  ljc(G,[B|Vs2]).


ljc_imp((C->D),B,Vs):-!,ljc((C->D),[(D->B)|Vs]).
ljc_imp(A,_,Vs):-memberchk(A,Vs).   


tprove(F):-toImp(F,I),dneg(I,NNI),kprove(NNI).

toImp(X,R):-atomic(X),!,R=X.
toImp((X->Y),(A->B)):-toImp(X,A),toImp(Y,B).

toImp(~(X),(A->false)):-toImp(X,A).
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
  

% simple evaluator / truth table generator
% for classic  formulas

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
evalT(true,R):-!,R=1.
evalT(X,R):-integer(X),!,R=X.
evalT(~A,R):- evalT(A,X),neg(X,R).
evalT((A->B),R):-
  evalT(A,X),
  evalT(B,Y),
  impl(X,Y,R).
evalT((A&B),R):-
  evalT(A,X),
  evalT(B,Y),
  conj(X,Y,R).
evalT((A v B),R):-
  evalT(A,X),
  evalT(B,Y),
  disj(X,Y,R).
evalT((A<->B),R):-
  evalT(A,X),
  evalT(B,Y),
  equiv(X,Y,R).
evalT((A<-B),R):-
  evalT(A,X),
  evalT(B,Y),
  rev_impl(X,Y,R).

neg(0,1).
neg(1,0).

impl(0,0,1).
impl(0,1,1).
impl(1,0,0).
impl(1,1,1).

conj(0,0,0).
conj(0,1,0).
conj(1,0,0).
conj(1,1,1).

disj(0,0,0).
disj(0,1,1).
disj(1,0,1).
disj(1,1,1).

equiv(0,0,1).
equiv(0,1,0).
equiv(1,0,0).
equiv(1,1,1).

rev_impl(0,0,1).
rev_impl(0,1,1).
rev_impl(1,0,0).
rev_impl(1,1,1).

bit(0).
bit(1).

% if taut succeeds -> all vars the same succeeds
% if all vars the same fails, tautology cannot succeed
fforce(A):-
  flag(fval,_,0),force(A),
  flag(fval,_,1),force(A).
  
force(A & B):-!,force(A),force(B).
force(~A):-!,\+force(A).
force(A->B):-!, (\+force(A);force(B)),!.
force(A v B):-!,force(~(~A & ~B)).
force(A <-> B):-!,force(A->B),force(B->A).
force(_):-flag(fval,X,X),X=1.




