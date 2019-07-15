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

/*

% tableau prover - Fitting - still buggy


%ftprove(F):- \+ ((treduce([f(F)],Xs),ppp(Xs),open(Xs))).

ftprove(F):- forall(
  (treduce([f(F)],Xs)),
  closed(Xs)
).

closed(Xs):-select(t(A),Xs,Ys),memberchk(f(A),Ys),!.


%treduce([X|_],_):-ppp(X),fail.
treduce([],[]).
treduce([t(A)|Xs],[t(A)|Ys]):-atomic(A),treduce(Xs,Ys).
treduce([f(A)|Xs],[f(A)|Ys]):-atomic(A),treduce(Xs,Ys).
treduce([f(A->B)|Xs],Zs):-ftrim(Xs,Ys),treduce([t(A),f(B)|Ys],Zs).  
treduce([t(A->_B)|Xs],Ys):-treduce([f(A)|Xs],Ys).
treduce([t(_A->B)|Xs],Ys):-treduce([t(B)|Xs],Ys).


ftrim([],[]).
ftrim([f(_)|Xs],Ys):-ftrim(Xs,Ys).
ftrim([t(X)|Xs],[t(X)|Ys]):-ftrim(Xs,Ys).

% incmpleteness: should succeed
ftbug:-ftprove((((0->1)->2)->1->2)).

%is_closed(t(X),Xs):-memberchk(f(X),Xs).
%is_closed(f(X),Xs):-memberchk(t(X),Xs).

*/






