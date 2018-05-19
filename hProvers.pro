% works on Horn clauses - includes
% preprocessing from implicational form
% from which the translation is reversible

hprove(T0):-toHorn(T0,T),ljh(T,[]),!.

ljh(A):-ljh(A,[]),!.

ljh(A,Vs):-memberchk(A,Vs),!. 
ljh((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljh(B,Vs2).
ljh(G,Vs1):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  ljh_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim empty bodies
  ljh(G,[NewB|Vs2]).
  
ljh_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljh_imp((D:-Cs),B,Vs):- ljh((D:-Cs),[(B:-[D])|Vs]).

trimmed((B:-[]),R):-!,R=B.
trimmed(BBs,BBs).


hhprove(T0):-toSortedHorn(T0,T),ljh(T,[]),!.

timed_hprove(T):-timed_hprove(600,T).

timed_hprove(Max,T):-
  timed_call(Max,hprove(T),Time),
  (compound(Time)->ppp(Time:T);true).
  

fprove(T0):-toListHorn(T0,T),ljf(T,[]),!.

ljf(A,Vs):-memberchk(A,Vs),!. 
ljf([B|As],Vs1):-!,append(As,Vs1,Vs2),ljf(B,Vs2).
ljf(G,Vs1):- % atomic(G), G not on Vs
  memberchk([G|_],Vs1),
  select([B|As],Vs1,Vs2),
  select(A,As,Bs), 
  ljf_imp(A,B,Vs2), % A element of the body of B
  !,
  ftrimmed([B|Bs],NewB),
  ljf(G,[NewB|Vs2]).
  
ljf_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljf_imp([D|Cs],B,Vs):- ljf([D|Cs],[[B,D]|Vs]).

ftrimmed([B],R):-!,R=B.
ftrimmed(BBs,BBs).


% fastest on bm/2, at this point
vprove(T0):-toListHorn(T0,T),ljv(T,[]),!.

ljv(A,Vs):-memberchk(A,Vs),!. 
ljv([B|As],Vs1):-!,append(As,Vs1,Vs2),ljv(B,Vs2).
ljv(G,Vs1):-
  memberchk([G|_],Vs1),
  ljv_choice(G,Vs1,End,End).
  
ljv_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljv_imp([D|Cs],B,Vs):- ljv([D|Cs],[[B,D]|Vs]).

ljv_choice(G,[[B|As]|End],Vs2,End):-
  select(A,As,Bs),
  ljv_imp(A,B,Vs2),
  !,
  vtrimmed(Bs,B,G,Vs2).
ljv_choice(G,[Ys|Vs1],Vs2,End):-
  ljv_choice(G,Vs1,[Ys|Vs2],End).


vtrimmed([],B,G,Vs):-ljv(G,[B|Vs]).
vtrimmed([BB|Bs],B,G,Vs):-ljv(G,[[B,BB|Bs]|Vs]).

% works on Horn clauses - includes
% preporcessing from implicational form
% from which the translation is reversible except for order


xprove(T0):-toHorn(T0,T),ljy(T,[]),!.

yprove(T0):-toSortedHorn(T0,T),ljy(T,[]),!.


ljy(A,Vs):-memberchk(A,Vs),!. 
ljy((B:-[B]),_):-!.
ljy((B:-As),Vs1):-!,add_all(As,Vs1,Vs2),ljy(B,Vs2).
ljy(G,Vs1):- % atomic(G), G not on Vs
  select((B:-As),Vs1,Vs2),
  select(A,As,Bs), 
  ljy_imp(A,B,Vs2),
  !,
  trimmed((B:-Bs),NewB),
  add_new(NewB,Vs2,Vs3),
  ljy(G,Vs3).
  
ljy_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljy_imp((D:-Cs),B,Vs1):-
   add_new((B:-[D]),Vs1,Vs2),
   ljy((D:-Cs),Vs2).


% variant of xprove, with nondeterministic part
% confined to zreduce/2

zprove_init(T0):-toSortedHorn(T0,_). % baseline for benchmarks

zprove(T0):-toSortedHorn(T0,T),ljz(T,[]),!.

ljz(A,Vs):-memberchk(A,Vs),!. 
ljz((B:-[B]),_):-!.
ljz((B:-As),Vs1):-!,add_all(As,Vs1,Vs2),ljz(B,Vs2).
ljz(G,Vs1):-zreduce(Vs1,Vs2),ljz(G,Vs2).

zreduce(Vs1,Vs3):-
  select((B:-As),Vs1,Vs2), % find clause with head B
  select(A,As,Bs),  % find A in its body
  ljz_imp(A,B,Vs2), % try to prove A using NewB
  !,                    % if A proven
  trimmed((B:-Bs),NewB), % if Bs=[] keep atom B, otherwise B:-Bs
  add_new(NewB,Vs2,Vs3). % extend env. with it

ljz_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs). % done, A is in Vs
ljz_imp((D:-Cs),B,Vs1):-
  add_new((B:-[D]),Vs1,Vs2), % assume that A's head implies B
  ljz((D:-Cs),Vs2).          % prove A under that assumption

hgprove(T0):-toHorn(T0,T),ljg(T,[]),!.

hhgprove(T0):-toSortedHorn(T0,T),ljg(T,[]),!.

ljg(A,Vs):-memberchk(A,Vs),!. 
ljg((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljg(B,Vs2).
ljg(G,Vs0):- % G is atomic
  select((G:-Gs),Vs0,Vs1),!, % bring a G:-.. first
  select((B:-As),[(G:-Gs)|Vs1],Vs2),
  select(A,As,Bs), 
  ljg_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB),
  ljg(G,[NewB|Vs2]).
  
ljg_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljg_imp((D:-Cs),B,Vs):- ljg((D:-Cs),[(B:-[D])|Vs]).

