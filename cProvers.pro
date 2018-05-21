% provers using embedded Horn Clauses or customized
% for also handling negation and classic proofs
% via Glivenko's translation from classical to intuitionistic
% propositional calculus

:- op(150,  fy,  ~ ). % negation

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

expand_neg(A,R):-atomic(A),!,R=A.
expand_neg(~A,R):-!,expand_neg(A,B),R=(B->false).
expand_neg((A->B),(X->Y)):-expand_neg(A,X),expand_neg(B,Y).


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
ljc((A->B),Vs1):-!,add_new(A,Vs1,Vs2),ljc(B,Vs2). 
ljc(G,Vs1):- % atomic(G),
  select((A->B),Vs1,Vs2),
  ljc_imp(A,B,Vs2),
  !,
  add_new(B,Vs2,Vs3),
  ljc(G,Vs3).


ljc_imp((C->D),B,Vs1):-!, 
   add_new((D->B),Vs1,Vs2),
   ljc((C->D),Vs2).
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

