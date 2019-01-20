obprove(T):-oljb(T,[]).

%oljb(A,Vs):-ppp((Vs-->A)),fail. % fo traing only

oljb(A,Vs):-memberchk(A,Vs),!.
oljb((A->B),Vs):-!,oljb(B,[A|Vs]). 
oljb(G,Vs1):-
  select((A->B),Vs1,Vs2),
  oljb_imp(A,B,Vs2),
  !,
  oljb(G,[B|Vs2]).

oljb_imp((C->D),B,Vs):-!,oljb((C->D),[(D->B)|Vs]).
oljb_imp(A,_,Vs):-memberchk(A,Vs).   
