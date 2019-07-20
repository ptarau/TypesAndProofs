fmprove(G):-
  expand_full_neg(G,F),
  mints(F,H,Bs),
  %length(Bs,L),ppp(H:L),
  %ppp(Bs->H),
  %ppp(H),
  ljfa(H,Bs).
  
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

flprove(T):-
  flattenFull(T,FT,Vs),
  faprove(FT,Vs),
  !.
  
faprove(T0,Vs):-
  unexpand(Vs,T0,T),
  expand_full_neg(T,FullT),
  %ppp(FullT),
  ljfa(FullT,[]),!.

% works with hardened disj-eq-neg formulas
fxprove(T0,Vs):-
  unexpand(Vs,T0,T1),
  simplify(T1,T),
  expand_full_neg(T,FullT),
  %ppp(FullT),
  ljfa(FullT,[]),!.
  
  
faprove1(T0,Vs0):-
  maplist(expand_full_neg,[T0|Vs0],[T|Vs]),
  %ppp(T),
  ljfa(T,Vs),
  !.

  
  

fxprove(T0):-
  simplify(T0,T1),
  expand_full_neg(T1,T),
  %ppp(here=T),
  ljfa(T,[]),
  !.    
  
max_steps(100). 
:-flag(steps,_,0).

spy(Mes,T,Vs):-
	max_steps(M),
	flag(steps,K,K),
	( K<M->flag(steps,_,K+1),
	  ppp(Mes/K:(T:-Vs))
	; flag(steps,_,0),
	  ppp(done),
		abort
	).	


faprove(T0):-
  expand_full_neg(T0,T),
  %ppp(here=T),
  ljfa(T,[]),
  !.    
  
ljfa(T):-  ljfa(T,[]).

%ljfa(G,Vs):-spy('ljf',G,Vs),fail.
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

%ljfa_reduce(AB,G,Vs,Vs):-compound(AB),ppp(reduce(G):(AB:-Vs)),fail.
ljfa_reduce((A->B),_,Vs1,Vs2):-!,ljfa_imp(A,B,Vs1,Vs2).
ljfa_reduce((A & B),_,Vs,[A,B|Vs]):-!.
ljfa_reduce((A<->B),_,Vs,[(A->B),(B->A)|Vs]):-!.
ljfa_reduce((A v B),G,Vs,[B|Vs]):-ljfa(G,[A|Vs]).
 
% alternative, avoidng duplication of D
%ljfa_imp((C->D),B,Vs,[B|Vs]):-!,gensym(p__,P),ljfa(P,[C,(D->P),(P->B)|Vs]).
ljfa_imp((C->D),B,Vs,[B|Vs]):-!,ljfa((C->D),[(D->B)|Vs]).

ljfa_imp((C & D),B,Vs,[(C->(D->B))|Vs]):-!.
ljfa_imp((C v D),B,Vs,[(C->B),(D->B)|Vs]):-!.
ljfa_imp((C<->D),B,Vs,[((C->D)->((D->C)->B))|Vs]):-!.
ljfa_imp(A,B,Vs,[B|Vs]):-memberchk(A,Vs).  

fdprove(T0):-
  expand_full_neg(T0,T),
  %ppp(here=T),
  ljfd(T,[]),
  !.    
    
ljfd(T):-  ljfd(T,[]).

%ljfd(A,Vs):-ppp(ljfd:(Vs-->A)),fail. % fo traing only

ljfd(A,Vs):-memberchk(A,Vs),!.
ljfd(_,Vs):-memberchk(false,Vs),!.
ljfd(A<->B,Vs):-!,ljfd(B,[A|Vs]),ljfd(A,[B|Vs]).
ljfd((A->B),Vs):-!,ljfd(B,[A|Vs]).
ljfd(A & B,Vs):-!,ljfd(A,Vs),ljfd(B,Vs).
ljfd(G,Vs):-ljfd_sel(G,Vs).
  
ljfd_sel(G,Vs1):- %atomic(G),% atomic or disj or false
  select(Red,Vs1,Vs2),
  ljfd_reduce(Red,G,Vs2,Vs3),
  !,
  ljfd(G,Vs3).
ljfd_sel(A v B, Vs):-(ljfd(A,Vs);ljfd(B,Vs)),!.
  
%ljfd_reduce(AB,B,Vs,Vs):-ppp(ljfd_reduce:(vs:Vs-->ab:AB+b:B)),fail. 
ljfd_reduce((A->B),_,Vs1,Vs2):-!,ljfd_imp(A,B,Vs1,Vs2).
ljfd_reduce((A & B),_,Vs,[A,B|Vs]):-!.
ljfd_reduce((A<->B),_,Vs,[(A->B),(B->A)|Vs]):-!.
ljfd_reduce((A v B),G,Vs,[B|Vs]):-ljfd(G,[A|Vs]).
  
ljfd_imp((C->D),B,Vs,[B|Vs]):-!,ljfd((C->D),[(D->B)|Vs]).
ljfd_imp((C & D),B,Vs,[(C->(D->B))|Vs]):-!.
ljfd_imp((C v D),B,Vs,[(C->B),(D->B)|Vs]):-!.
ljfd_imp((C<->D),B,Vs,[((C->D)->((D->C)->B))|Vs]):-!.
ljfd_imp(A,B,Vs,[B|Vs]):-memberchk(A,Vs).  


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
ljfc(G,Vs1):- % atomic or false or disjunction !
  select(Red,Vs1,Vs2),
  ljfc_reduce(Red,G,Vs2,Vs3),
  !,
  ljfc(G,Vs3).
  
ljfc_reduce((C->D)->B,_G,Vs,[B|Vs]):-!,ljfc((C->D),[(D->B)|Vs]).
ljfc_reduce((C & D)->B,_G,Vs,[(C->(D->B))|Vs]):-!.
ljfc_reduce((C v D)->B,_G,Vs,[(C->B),(D->B)|Vs]):-!.
ljfc_reduce((C<->D)->B,_G,Vs,[((C->D)->((D->C)->B))|Vs]):-!.
ljfc_reduce((A & B),_G,Vs,[A,B|Vs]):-!.
ljfc_reduce((A<->B),_G,Vs,[(A->B),(B->A)|Vs]):-!.
ljfc_reduce((A v B),G,Vs,[B|Vs]):-!,ljfc(G,[A|Vs]).
ljfc_reduce((A->B),_G,Vs,[B|Vs]):-memberchk(A,Vs).



frprove(T):-
  flattenFull(T,FT,Vs),
  %show_expanded(T,Vs),
  ljff(FT,Vs),
  !.
 
%ljff(A,Vs):-show_expanded(A,Vs),fail.
ljff(A,Vs):-memberchk(A,Vs),!.
ljff(_,Vs):-memberchk(false,Vs),!.
ljff((A->B),Vs):-!,ljff(B,[A|Vs]). 
ljff(A v B, Vs):-(ljff(A,Vs);ljff(B,Vs)),!.
ljff(G,Vs0):- % atomic or disjunction !
  sort(Vs0,Vs1),
  select(Red,Vs1,Vs2),
  ljff_reduce(Red,G,Vs2,Vs3),
  !,
  ljff(G,Vs3).
  
ljff_reduce((C->D)->B,_G,Vs,[B|Vs]):-!,ljff((C->D),[(D->B)|Vs]).
ljff_reduce((A v B),G,Vs,[B|Vs]):-!,ljff(G,[A|Vs]).
ljff_reduce((A->B),_G,Vs,[B|Vs]):-memberchk(A,Vs).


expand_full_neg(A,R):-expand_full_neg(A,R,_,[]).

expand_full_neg(A,R,Ops):-
  expand_full_neg(A,R,Os,[]),
  sort(Os,Ops).

% f seems to mean just an atom, not false in the tests
%expand_full_neg(f,R)-->{!,R=false}. % DO NOT UNCOMMENT!
expand_full_neg(false,R)-->!,{R=false}.
expand_full_neg(true,R)-->!,{R=true}.
expand_full_neg(A,R)-->{primitive(A),!,R= '?'(A)}.
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
 

fb_filter(G):-check_ops_in([(->)/2,(<->)/2],G). 
 
i_filter(G):-check_ops_in([(->)/2],G). 

fbprove(T):-ljfb(T,[]),!.
 
% for nested Horn (all but disj)
nest_filter(G):-
  check_ops_in([(->)/2,(<->)/2,(&)/2,(~)/1],G). 

  
hand_filter(G):-
  check_ops_in([(->)/2,(&)/2,(<->)/2,(~)/1],G).

  
ljfb(A,Vs):-memberchk(A,Vs),!.
ljfb((A->B),Vs):-!,ljfb(B,[A|Vs]). 
ljfb(A <-> B,Vs):-!,ljfb((A->B),Vs),ljfb((B->A),Vs).
ljfb(G,Vs1):-  
  member(T,Vs1),eq_head_of(T,G),!,
  select(Red,Vs1,Vs2),
  ljfb_reduce(Red,Vs2,Vs3),
  !,
  ljfb(G,Vs3).



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
