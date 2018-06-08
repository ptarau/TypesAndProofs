faprove(T0):-
  expand_neg(T0,T),
  ljfa(T,[]),!.

%ljfa(A,Vs):-ppp((Vs-->A)),fail. % fo traing only

ljfa(A,Vs):-memberchk(A,Vs),!.
ljfa(_,Vs):-memberchk(false,Vs),!.
ljfa((A->B),Vs):-!,ljfa(B,[A|Vs]). 
ljfa(A & B,Vs):-!,ljfa(A,Vs),ljfa(B,Vs).
ljfa(A v B, Vs):-!,ljfa(A,Vs);ljfa(B,Vs).
ljfa(G,Vs1):-
  select(Red,Vs1,Vs2),
  reduce(Red,Vs2,Vs3),
  !,
  ljfa(G,Vs3).

reduce((A->B),Vs1,Vs2):-!,  
  ljfa_imp(A,B,Vs1,Vs2).
reduce((A&B),Vs1,Vs2):-!,   
  reduce([A,B|Vs1],Vs2).
reduce((A v B),Vs1,Vs3):-!,   
  reduce([A|Vs1],Vs2),
  reduce([B|Vs2],Vs3).
  
ljfa_imp(A,B,Vs,[B|Vs]):-atomic(A),!,memberchk(A,Vs).    
ljfa_imp((C-> D),B,Vs,[B|Vs]):-!,ljfa((C->D),[(D->B)|Vs]).
ljfa_imp((C & D),B,Vs,[(C->(D->B))|Vs]):-!.
ljfa_imp((C v D),B,Vs,[((C->B),(D->B))|Vs]).

 
