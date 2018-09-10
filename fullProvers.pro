

  
fftprove(T0):-
  tautology(T0), % calls Fitting's prover to filter out some non-tautolgies
  expand_full_neg(T0,T),
  %ppp(T),
  ljfa(T,[]),!.  
  
ffprove(T0):-
  force(T0), % calls trival class/intu taut checker with all vars same
  expand_full_neg(T0,T),
  %ppp(T),
  ljfa(T,[]),!.  
  
faprove(T0,Vs):-
  unexpand(Vs,T0,T),
  expand_full_neg(T,FullT),
  %ppp(FullT),
  ljfa(FullT,[]),!.

faprove1(T0,Vs0):-
  maplist(expand_full_neg,[T0|Vs0],[T|Vs]),
  %ppp(T),
  ljfa(T,Vs),
  !.

  
faprove(T0):-
  expand_full_neg(T0,T),
  %ppp(here=T),
  ljfa(T,[]),
  !.    
    
ljfa(T):-  ljfa(T,[]).

%ljfa(A,Vs):-ppp(ljfa:(Vs-->A)),fail. % fo traing only

ljfa(A,Vs):-memberchk(A,Vs),!.
ljfa(_,Vs):-memberchk(false,Vs),!.
ljfa(A<->B,Vs):-!,ljfa(B,[A|Vs]),ljfa(A,[B|Vs]).
ljfa((A->B),Vs):-!,ljfa(B,[A|Vs]).
ljfa(A & B,Vs):-!,ljfa(A,Vs),ljfa(B,Vs).
ljfa(G,Vs1):- % atomic or disj or false
  select(Red,Vs1,Vs2),
  ljfa_reduce(Red,G,Vs2,Vs3),
  !,
  ljfa(G,Vs3).
ljfa(A v B, Vs):-(ljfa(A,Vs);ljfa(B,Vs)),!.

%ljfa_reduce(AB,B,Vs,Vs):-ppp(ljfa_reduce:(vs:Vs-->ab:AB+b:B)),fail. 
ljfa_reduce((A->B),_,Vs1,Vs2):-!,ljfa_imp(A,B,Vs1,Vs2).
ljfa_reduce((A & B),_,Vs,[A,B|Vs]):-!.
ljfa_reduce((A<->B),_,Vs,[(A->B),(B->A)|Vs]):-!.
ljfa_reduce((A v B),G,Vs,[B|Vs]):-ljfa(G,[A|Vs]).
  
ljfa_imp((C->D),B,Vs,[B|Vs]):-!,ljfa((C->D),[(D->B)|Vs]).
ljfa_imp((C & D),B,Vs,[(C->(D->B))|Vs]):-!.
ljfa_imp((C v D),B,Vs,[(C->B),(D->B)|Vs]):-!.
ljfa_imp((C<->D),B,Vs,[((C->D)->((D->C)->B))|Vs]):-!.
ljfa_imp(A,B,Vs,[B|Vs]):-memberchk(A,Vs).  

fcprove(T):-fcprove(T,[]).

fcprove(T0,Vs):-
  unexpand(Vs,T0,T),
  expand_full_neg(T,FullT),
  ljfc(FullT,[]),
  !.
  
ljfc(A,Vs):-memberchk(A,Vs),!.
ljfc(_,Vs):-memberchk(false,Vs),!.
ljfc((A->B),Vs):-!,ljfc(B,[A|Vs]). 
ljfc(A <-> B,Vs):-!,ljfc((A->B),Vs),ljfc((B->A),Vs).
ljfc(A & B,Vs):-!,ljfc(A,Vs),ljfc(B,Vs).
ljfc(A v B, Vs):-(ljfc(A,Vs);ljfc(B,Vs)),!.
ljfc(G,Vs1):- % atomic or disjunction !
  select(Red,Vs1,Vs2),
  ljfc_reduce(Red,G,Vs2,Vs3),
  !,
  ljfc(G,Vs3).
  

ljfc_reduce((A    ->B),_G,Vs,[B|Vs]):-atomic(A),!,memberchk(A,Vs).
ljfc_reduce((C->D)->B,_G,Vs,[B|Vs]):-ljfc((C->D),[(D->B)|Vs]).

ljfc_reduce((C & D)->B,_G,Vs,[(C->(D->B))|Vs]).
ljfc_reduce((C v D)->B,_G,Vs,[(C->B),(D->B)|Vs]).
ljfc_reduce((C<->D)->B,_G,Vs,[((C->D)->((D->C)->B))|Vs]).

ljfc_reduce((A & B),_G,Vs,[A,B|Vs]).
ljfc_reduce((A<->B),_G,Vs,[(A->B),(B->A)|Vs]). 
ljfc_reduce((A v B),G,Vs,[B|Vs]):-ljfc(G,[A|Vs]).




expand_full_neg(A,R):-expand_full_neg(A,R,_,[]).

expand_full_neg(A,R,Ops):-
  expand_full_neg(A,R,Os,[]),sort(Os,Ops).

  % f seema to mean just an atom, not false in the tests
%expand_full_neg(f,R)-->{!,R=false}. % DO NOT UNCOMMENT!
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


nobug2:-faprove(((0->0)->false)->0).

check_ops_in(_Ops,A):-atomic(A),!.
check_ops_in(Ops,A):-
 A=..[F|Xs],member(F/N,Ops),length(Xs,N),
 !,
 maplist(check_ops_in(Ops),Xs).
 

fb_filter((G:-Vs)):-
  maplist(check_ops_in([(->)/2,(<->)/2]),[G|Vs]). 
 
fbprove(T):-ljfb(T,[]),!.
  
ljfb(A,Vs):-memberchk(A,Vs),!.
ljfb((A->B),Vs):-!,ljfb(B,[A|Vs]). 
ljfb(A <-> B,Vs):-!,ljfb((A->B),Vs),ljfb((B->A),Vs).
ljfb(G,Vs1):-  
  member(T,Vs1),eq_head_of(T,G),!,
  select(Red,Vs1,Vs2),
  ljfb_reduce(Red,Vs2,Vs3),
  !,
  ljfb(G,Vs3).


expand_equiv(X<->Y,Vs,[(X->Y),(Y->X)|Vs]):-!.
expand_equiv(_,Vs,Vs).

eq_head_of((_->B), G) :- !,eq_head_of(B, G).
eq_head_of((A<->B), G) :- (eq_head_of(A,G);eq_head_of(B,G)),!.
eq_head_of(G, G).  
  
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
