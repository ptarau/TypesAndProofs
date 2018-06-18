faprove(T0,Vs):-
  unexpand(Vs,T0,T),
  expand_full_neg(T,FullT),
  ljfa(FullT,[]),!.

faprove1(T0,Vs0):-
  maplist(expand_full_neg,[T0|Vs0],[T|Vs]),
  %ppp(T),
  ljfa(T,Vs),
  !.

faprove(T0):-
  expand_full_neg(T0,T),
  %ppp(T),
  ljfa(T,[]),!.  
  
expand_full_neg(A,R):-expand_full_neg(A,R,_,[]).

expand_full_neg(A,R,Ops):-expand_full_neg(A,R,Os,[]),sort(Os,Ops).

%expand_full_neg(f,R)-->{!,R=false}.
expand_full_neg(A,R)-->{atomic(A),!,R=A}.
expand_full_neg(~(A),(B->false))-->!,[(~)],
  expand_full_neg(A,B).
expand_full_neg(A,B)-->
 {A=..[F|Xs]},
 [F],
 expand_full_negs(Xs,Ys),
 {B=..[F|Ys]}.

 
expand_full_negs([],[])-->[].
expand_full_negs([X|Xs],[Y|Ys])-->expand_full_neg(X,Y),
  expand_full_negs(Xs,Ys).
 
% turns Vs,G into V1->V2->...->G 
unexpand([],G,G).
unexpand([V|Vs],G,(V->R)):-unexpand(Vs,G,R).
  

%ljfa(A,Vs):-ppp((Vs-->A)),fail. % fo traing only

ljfa(A,Vs):-memberchk(A,Vs),!.
ljfa(_,Vs):-memberchk(false,Vs),!.
ljfa((A->B),Vs):-!,ljfa(B,[A|Vs]). 
ljfa(G,Vs1):-
  %member(T,Vs1),head_of(T,G),!, % TODO, for all operators
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


nobug2:-faprove(((0->0)->false)->0).


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


% adaptor for Dyckhoff's original full prover



dyprove(G,[]):-!,dprove(G).  
dyprove(G,Vs):-axs2conj(Vs,Cs),dprove((Cs->G)).

axs2conj([X],R):-!,R=X.
axs2conj([X|Xs],X & Cs):-axs2conj(Xs,Cs).

