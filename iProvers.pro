% minimal logic prover, via generate and test

intu(T):-intu(10,T,_).

intu(T0,X):-intu(16,T0,X).

intu(N,T0,X):-intu(N,T0,X,_),!.

intu(N,T0,X,T):-tnfs(N,X,T),subsumes_term(T,T0).

intu0(T0,X):-N=16,intu0(N,T0,X).

intu0(N,T0,X):-tnfs(N,X,T),T0=@=T.

badprove(_) :- 0 =:= random(2).
  
% Dyckhoff's original LJT   
dprove(A):-provable(A),!.
  
% Dyckhoff's LJT - fig 2  
   
lprove(T):-ljt(T,[]),!.


ljt(A,Vs):-memberchk(A,Vs),!.     % axiom

ljt((A->B),Vs1):-
  !,
  add_new(A,Vs1,Vs2),
  ljt(B,Vs2).                           % => imp 

ljt(G,Vs1):- %atomic(G),                % imp => 4
  select( ((C->D)->B),Vs1,Vs2),
  add_new((D->B),Vs2,Vs3),
  ljt((C->D), Vs3),
  !,
  add_new(B,Vs2,Vs4),
  ljt(G,Vs4).
  
ljt(G,Vs1):- %atomic(G),                % imp => 1, atom A
  select((A->B),Vs1,Vs2),
  atomic(A),
  memberchk(A,Vs2),
  !,
  add_new(B,Vs2,Vs3),
  ljt(G,Vs3).

add_new(X,Xs,Ys):-memberchk(X,Xs),!,Ys=Xs.
add_new(X,Xs,[X|Xs]).
  



% simplest, with multisets, no contraction

bprove(T0):-trimImps(T0,T),ljb(T,[]),!.

ljb(A,Vs):-memberchk(A,Vs),!.
ljb((A->B),Vs):-!,ljb(B,[A|Vs]). 
ljb(G,Vs1):-
  select((A->B),Vs1,Vs2),
  ljb_imp(A,B,Vs2),
  !,
  ljb(G,[B|Vs2]).

ljb_imp(A,_,Vs):-atomic(A),!,memberchk(A,Vs).   
ljb_imp((C->D),B,Vs):-ljb(D,[(D->B),C|Vs]).



%%%%%%%%%%%%%%

pprove(T):-ljp(T,[]),!.

ljp(A,Vs):-memberchk(A,Vs),!.
ljp((A->B),Vs1):-!,add_new(A,Vs1,Vs2),ljp(B,Vs2). 
ljp(G,Vs1):- % atomic(G),
  select((A->B),Vs1,Vs2),
  ljp_imp(A,B,Vs2),
  !,
  add_new(B,Vs2,Vs3),
  ljp(G,Vs3).

ljp_imp(A,_,Vs):-atomic(A),!,memberchk(A,Vs).   
ljp_imp((C->D),B,Vs1):-
   add_new((D->B),Vs1,Vs2),
   ljp((C->D),Vs2).
   
mprove(T):-ljm(T,[]),!.

ljm(A,Vs):-memberchk(A,Vs),!.
ljm((A->B),Vs1):-!,
  add_new(A,Vs1,Vs2),
  ljm(B,Vs2). 
ljm(G,Vs):- % atomic(G),
  select_imp(G,Vs,Us,Us).
  
ljm_imp(A,_,Vs):-atomic(A),!,memberchk(A,Vs).   
ljm_imp((C->D),B,Vs1):-
  add_new((D->B),Vs1,Vs2),
  ljm((C->D),Vs2).

select_imp(G,[(A->B)|Vs],Us1,Vs):-
  ljp_imp(A,B,Us1),
  !,
  add_new(B,Us1,Us2),
  ljm(G,Us2).
select_imp(G,[U|Vs],Us,End):-
  select_imp(G,Vs,[U|Us],End).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
ljs(X,T):-ljs(X,T,[]),!.

ljs(X,A,Vs):-memberchk(X:A,Vs),!. %----

ljs(l(X,E),(A->B),Vs):-!,ljs(E,B,[X:A|Vs]).   % ---

ljs(E,G,Vs1):- % atomic(G),
  select(S:(A->B),Vs1,Vs2),
  ljs_imp(T,A,B,Vs2),
  !,
  ljs(E,G,[a(S,T):B|Vs2]).

ljs_imp(X,A,_,Vs):-atomic(A),!,memberchk(X:A,Vs).   
ljs_imp(E,(C->D),B,Vs1):-ljs(E,(C->D),[_:(D->B)|Vs1]).


%%%%%%%%%%%%%%

sprove(T):-sprove(T,_).

sprove(T,X):-ljs(X,T,[]),!.


  