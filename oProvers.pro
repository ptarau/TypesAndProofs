% other, possibly incorrect provers

obprove(T):-oljb(T,[]).

%oljb(A,Vs):-ppp((Vs-->A)),fail. % for trainig only

oljb(A,Vs):-memberchk(A,Vs),!.
oljb((A->B),Vs):-!,oljb(B,[A|Vs]). 
oljb(G,Vs1):-
  select((A->B),Vs1,Vs2),
  oljb_imp(A,B,Vs2),
  !,
  oljb(G,[B|Vs2]).

oljb_imp((C->D),B,Vs):-!,oljb((C->D),[(D->B)|Vs]).
oljb_imp(A,_,Vs):-memberchk(A,Vs).   

cmprove(E):-
  cmints(E,H,Gs),
  %ppp((H:-Gs)),
  %maplist(simplify,Gs,Bs), 
  ppp(E),nl,ppp((H:-Gs)),nl,
  ljfa(H,Gs).

/*
  
% still buggy attempt to write prover in terms of <->, v, and false 
  
bdprove(T):-expand_full_neg(T,A),toDisjBiCond(A,B),ljbd(B).

ljbd(T):-  ljbd(T,[]).

%ljbd(G,Vs):-spy('ljf',G,Vs),fail.
ljbd(A,Vs):-memberchk(A,Vs),!.
ljbd(_,Vs):-memberchk(false,Vs),!.
ljbd(A<->B,Vs):-!,ljbd(B,[A|Vs]),ljbd(A,[B|Vs]).
%ljbd((A->B),Vs):-!,ljbd(B,[A|Vs]).
ljbd((AB),Vs):-is_impl(AB,A,B),!,ljbd(B,[A|Vs]).
%ljbd(A & B,Vs):-!,ljbd(A,Vs),ljbd(B,Vs).
ljbd(AB,Vs):-is_conj(AB,A,B),!,ljbd(A,Vs),ljbd(B,Vs).
ljbd(G,Vs1):- % atomic or disj or false
  select(Red,Vs1,Vs2),
  ljbd_reduce(Red,G,Vs2,Vs3),
  !,
  ljbd(G,Vs3).
ljbd(A v B, Vs):-(ljbd(A,Vs);ljbd(B,Vs)),!.

ljbd_reduce((AB),_,Vs1,Vs2):-is_impl(AB,A,B),!,ljbd_imp(A,B,Vs1,Vs2).
%ljbd_reduce((A & B),_,Vs,[A,B|Vs]):-!.
ljbd_reduce((AB),_,Vs,[A,B|Vs]):-is_conj(AB,A,B),!.
%ljbd_reduce((A<->B),_,Vs,[(A->B),(B->A)|Vs]):-!.
ljbd_reduce((A<->B),_,Vs,[(A<->B)|Vs]):-!.
ljbd_reduce((A v B),G,Vs,[B|Vs]):-ljbd(G,[A|Vs]).
 

ljbd_imp((CD),B,Vs,[B|Vs]):-is_impl(CD,_C,D),!,ljbd((CD),[((D v B)<->B)|Vs]).

%ljbd_imp((C & D),B,Vs,[(C->(D->B))|Vs]):-!.

ljbd_imp((CD),B,Vs,[(C->(D->B))|Vs]):-is_conj(CD,C,D),!.
ljbd_imp((C v D),B,Vs,[(C->B),(D->B)|Vs]):-!.
%ljbd_imp((C<->D),B,Vs,[((C->D)->((D->C)->B))|Vs]):-!.
ljbd_imp(CD,B,Vs,[((CD v B)<->B)|Vs]):-CD=(_C<->_D),!.
ljbd_imp(A,B,Vs,[B|Vs]):-memberchk(A,Vs).  

is_impl(((X v Y)<->Y),X,Y).
is_impl(((Y v X)<->Y),X,Y).
is_impl((Y<->(X v Y)),X,Y).
is_impl((Y<->(Y v X)),X,Y).

is_conj((X v Y)<->(X<->Y),X,Y).
is_conj((Y v X)<->(X<->Y),X,Y).
is_conj((X<->Y)<->(X v Y),X,Y).
is_conj((Y<->X)<->(X v Y),X,Y).

*/


% intuitionistic tableau prover - Fitting 1969
% sound but apparently incomplete, at least when restricted
% to implicational fragment - see following examples
%  - possible bug in this implementation? 

% should succeed
rgo:-ftprove((((((0->1)->0)->0)->1)->1)).
rgo1:-ftprove((0->((((1->2)->1)->1)->2)->2)).
rgo2:-ftprove( (0->0->((((1->2)->1)->1)->2)->2)).
rgo3:-ftprove((0->0->0->((((1->2)->1)->1)->2)->2)).
rgo4:-ftprove((0->0->0->0->((((1->2)->1)->1)->2)->2)).
rgo5:-ftprove((0->0->1->0->((((2->3)->2)->2)->3)->3)).
rgo6:-ftprove((0->1->2->3->((((4->5)->4)->4)->5)->5)).
rgo7:-ftprove(0->0->1->2->1->((((3->4)->3)->3)->4)->4).
rgo8:-ftprove(0->1->2->3->4->((((5->6)->5)->5)->6)->6).
rgo9:-ftprove(0->1->2->3->4->5->((((6->7)->6)->6)->7)->7).

unsound_ft(N,F):-allImpFormulas(N,F),ftprove(F),\+eprove(F).


incomplete_ft(N,F):-allImpFormulas(N,F),eprove(F),\+ftprove(F).


% prover

ftprove(X):-redstart(X,_Rss),!.

redstart(X,Rss):-redloop([[f:X]],Rss).

redend(Fss):-
 forall(
    member(Fs,Fss),
    closed(Fs)
 ).

closed(Xs):-select(t:A,Xs,Ys),memberchk(f:A,Ys),!. 
  
redloop(Fss,Rss):-%ppp(Fss),
  redend(Fss),!,Rss=Fss.
redloop(Fss,Rss):-redstep(Fss,Gss),redloop(Gss,Rss).

redstep(Fss,Hss):-
  select(Fs,Fss,Gss),
  select(X,Fs,Xs),
  ( X=f:(A->B),trim_fs(Xs,TXs),Gs=[t:A,f:B|TXs],Hss=[Gs|Gss]
  ; X=t:(A->B),Gs1=[f:A|Xs],Gs2=[t:B|Xs],Hss=[Gs1,Gs2|Gss]
  ).
  
trim_fs([],[]).
trim_fs([f:_|Xs],Ys):-trim_fs(Xs,Ys).
trim_fs([t:X|Xs],[t:X|Ys]):-trim_fs(Xs,Ys).


% using this breaks soundess 
trim_fs([],[],_).
trim_fs([f:(A->B)|Xs],[f:(A->B)|Ys],(f:(A->_))):-!,
  atomic(A),atomic(B),
  ppp(A->B),
  trim_fs(Xs,Ys,f:(A->B)).
trim_fs([f:X|Xs],Ys,Y):-ppp(trim=X),trim_fs(Xs,Ys,Y).
trim_fs([t:X|Xs],[t:X|Ys],Y):-trim_fs(Xs,Ys,Y).

  
  
