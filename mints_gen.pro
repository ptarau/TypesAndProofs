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
  \+ memberchk(G,Ms). % prevent trivial yes


bprove(T):-ljb(T,[]).

%ljb(A,Vs):-ppp((Vs-->A)),fail. % fo traing only

ljb(A,Vs):-memberchk(A,Vs),!.
ljb((A->B),Vs):-!,ljb(B,[A|Vs]).
ljb(G,Vs1):-
  select((A->B),Vs1,Vs2),
  ljb_imp(A,B,Vs2),
  !,
  ljb(G,[B|Vs2]).

ljb_imp((C->D),B,Vs):-!,ljb((C->D),[(D->B)|Vs]).
ljb_imp(A,_,Vs):-memberchk(A,Vs).


mints_show(N):-
  mints_gen(N,T),
  portray_clause(T),
  fail.
  
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

mints_provable(N,C):-
  C=(H:-Bs),
  mints_gen(N,C),
  ljb(H,Bs).
    
mints_failing(N,C):-
  C=(H:-Bs),
  mints_gen(N,C),
  \+ljb(H,Bs).


proof_test(N,[Yes,No,R]):-
   findall(X,mints_provable(N,X),Xs),
   length(Xs,Yes),
   findall(X,mints_failing(N,X),Ys),
   length(Ys,No),
   Yes+No>0,
   R is Yes/(Yes+No).
        
