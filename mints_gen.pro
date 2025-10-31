:-ensure_loaded('allPartitions.pro').
:-ensure_loaded('stats.pro').


mints_positive(P)-->[P].
mints_positive((P->Q))-->[P,Q].
mints_positive((P->(Q->R)))-->[P,Q,R].
mints_positive((P->Q)->R)-->[P,Q,R].

pos_mints_conjuncts([])-->[].
pos_mints_conjuncts([F|Fs])-->mints_positive(F),pos_mints_conjuncts(Fs).

mints_gen(N,(G:-Ms)):-
  length(Xs,N),
  natpartitions(Xs),
  [G|Ys]=Xs, %
  memberchk(G,Ys), % prevent trivial no
  pos_mints_conjuncts(Ms0,Ys,[]),
  no_dups_in(Ms0,Ms),
  \+ memberchk(G,Ms), % prevent trivial yes
  \+ memberchk((X->X),Ms).

bprove(T):-ljb(T,[]).

%ljb(A,Vs):-ppp((Vs-->A)),fail. % fo trainig only

ljb(A,Vs):-memberchk(A,Vs),!.
ljb((A->B),Vs):-!,ljb(B,[A|Vs]).
ljb(G,Vs1):-
  select((A->B),Vs1,Vs2),
  ljb_imp(A,B,Vs2),
  !,
  ljb(G,[B|Vs2]).

ljb_imp((C->D),B,Vs):-!,ljb((C->D),[(D->B)|Vs]).
ljb_imp(A,_,Vs):-memberchk(A,Vs).


gljb(G,Vs,Steps):-gljb(G,Vs,Steps,[]).


gljb(A,Vs)-->{memberchk(A,Vs)},!,[A].
gljb((A->B),Vs)-->!,gljb(B,[A|Vs]).
gljb(G,Vs1)-->
  {select((A->B),Vs1,Vs2)},
  gljb_imp(A,B,Vs2),
  !,
  gljb(G,[B|Vs2]).

gljb_imp((C->D),B,Vs)-->!,gljb((C->D),[(D->B)|Vs]),[(C->D)].
gljb_imp(A,_,Vs)-->{memberchk(A,Vs)},[A].


mints_show(N,Goal):-
  call(Goal,N,T),
  portray_clause(T),
  ppt(T),
  fail.

mints_show(N):-
  Goal=mints_provable,
  mints_show(N,Goal).

no_dups_in(Xs,Ys):-
    sort(Xs,Ys),
    length(Xs,L),
    length(Ys,L).

mints_count(N,Ks):-
  findall(K,
  (
  between(1,N,I),
  findall(X,mints_gen(I,X),Xs),
  length(Xs,K)
  ),
  Ks).

mints_provable(N,C+Steps):-
  C=(H:-Bs),
  mints_gen(N,C),
  gljb(H,Bs,Steps).

mints_failing(N,C):-
  C=(H:-Bs),
  mints_gen(N,C),
  \+gljb(H,Bs,_Steps).


proof_test(N,[all:All=yes:Yes+no:No,yes/all=R]):-
   findall(X,mints_provable(N,X),Xs),
   length(Xs,Yes),
   findall(X,mints_failing(N,X),Ys),
   length(Ys,No),
   All is Yes+No,
   All>0,
   R is Yes/(Yes+No).

proof_count(N):-
   between(1,N,I),
   proof_test(I,Result),
   portray_clause(I=Result),
   fail.
proof_count(_).

go__:-
  proof_count(8).
