faprove(T0):-
  expand_full_neg(T0,T),
  %ppp(T),
  ljfa(T,[]),!.

faprove(T0,Vs0):-
  maplist(expand_full_neg,[T0|Vs0],[T|Vs]),
  %ppp(T),
  ljfa(T,Vs),
  !.

%expand_full_neg(f,R):-!,R=false.
expand_full_neg(A,R):-atomic(A),!,R=A.
expand_full_neg(~(A),(B->false)):-!,
  expand_full_neg(A,B).
expand_full_neg(A,B):-
 A=..[F|Xs],
 maplist(expand_full_neg,Xs,Ys),
 B=..[F|Ys].
  
  
  
%ljfa(A,Vs):-ppp((Vs-->A)),fail. % fo traing only

ljfa(A,Vs):-memberchk(A,Vs),!.
ljfa(_,Vs):-memberchk(false,Vs),!.
ljfa((A->B),Vs):-!,ljfa(B,[A|Vs]). 
ljfa(G,Vs1):-
  select(Red,Vs1,Vs2),
  reduce(Red,G,Vs2,Vs3),
  !,
  ljfa(G,Vs3).
ljfa(A <-> B,Vs):-!,ljfa((A->B),Vs),ljfa((B->A),Vs).
ljfa(A & B,Vs):-!,ljfa(A,Vs),ljfa(B,Vs).
ljfa(A v B, Vs):-(ljfa(A,Vs);ljfa(B,Vs)).

  

%reduce(AB,B,Vs,Vs):-ppp(reduce:(vs:Vs-->ab:AB+b:B)),fail.  
reduce((A->B),_,Vs1,Vs2):-!,ljfa_imp(A,B,Vs1,Vs2).
reduce((A & B),_,Vs,[A,B|Vs]):-!.
reduce(A<->B,_,Vs,[(A->B),(B->A)|Vs]). 
reduce((A v B),G,Vs,[B|Vs]):-ljfa(G,[A|Vs]).


ljfa_imp(A,B,Vs,[B|Vs]):-atomic(A),!,memberchk(A,Vs).    
ljfa_imp((C-> D),B,Vs,[B|Vs]):-!,ljfa((C->D),[(D->B)|Vs]).
ljfa_imp((C & D),B,Vs,[(C->(D->B))|Vs]):-!.
ljfa_imp((C v D),B,Vs,[(C->B),(D->B)|Vs]).
ljfa_imp((C <-> D),B,Vs,[((C->D)->((D->C)->B))|Vs]).



fbprove(T0):-expand_full_neg(T0,T),ljfb(T,[]),!.
  
ljfb(A,Vs):-memberchk(A,Vs),!.
ljfb((A->B),Vs):-!,ljfb(B,[A|Vs]). 
ljfb(A <-> B,Vs):-!,ljfb((A->B),Vs),ljfb((B->A),Vs).
ljfb(G,Vs1):-
  select(Red,Vs1,Vs2),
  ljfb_reduce(Red,Vs2,Vs3),
  !,
  ljfb(G,Vs3).

  
  
ljfb_reduce((A->B),Vs1,Vs2):-!,ljfb_imp(A,B,Vs1,Vs2).
ljfb_reduce(A<->B,Vs,[(A->B),(B->A)|Vs]). 
  
ljfb_imp(A,B,Vs,[B|Vs]):-atomic(A),!,memberchk(A,Vs).    
ljfb_imp((C-> D),B,Vs,[B|Vs]):-!,ljfb((C->D),[(D->B)|Vs]).
ljfb_imp((C <-> D),B,Vs,[((C->D)->((D->C)->B))|Vs]).



