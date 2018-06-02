% works on Horn clauses - includes
% preprocessing from implicational form
% from which the translation is reversible

hprove(T0):-toHorn(T0,T),ljh(T).

ljh(A):-ljh(A,[]),!.

%ljh(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps
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

% transforms to an equational form, then depth at most 3 Horn

wprove(A):-toFlatHorn(A,B),ljh(B).


% seeing  hprove as working on Horn clauses with compound heads

hhprove(A):-toHorn(A,H),hlj(H),!.

hlj1(H):-hlj(H),!.

hlj(A:-Vs):-memberchk(A,Vs),!. 
hlj((B:-As):-Vs1):-!,append(As,Vs1,Vs2),hlj(B:-Vs2).
hlj(G:-Vs1):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  hlj_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim empty bodies
  hlj(G:-[NewB|Vs2]).
  
hlj_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
hlj_imp((D:-Cs),B,Vs):- hlj((D:-Cs):-[(B:-[D])|Vs]).



oprove(T0):-toHorn(T0,T),ljo(T).

ljo(A):-ljo(A,[]),!.

%ljo(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps
ljo(A,Vs):-memberchk(A,Vs),!. 
ljo((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljo(B,Vs2).
ljo(G,Vs1):- % atomic(G), G not on Vs1
  selsel(G,A,B,Bs,Vs1,Vs2),
  ljo_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim empty bodies
  ljo(G,[NewB|Vs2]).
  
ljo_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljo_imp((D:-Cs),B,Vs):- ljo((D:-Cs),[(B:-[D])|Vs]).

selsel(G,A,B,Bs,Vs1,Vs2):-
  memberchk((G:-_),Vs1),
  select((B:-As),Vs1,Vs2),
  select(A,As,Bs).
  
jprove(T0):-toHorn(T0,T),ljj(T).

ljj(A):-ljj(A,[]),!.

ljj(A,Vs):-memberchk(A,Vs),!. 
ljj((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljj(B,Vs2).
ljj(G,Vs0):- % atomic(G), G not on Vs1
  GGs=(G:-_),
  select(GGs,Vs0,Vs1), % if not, we just fail
  !,
  ( (B:-As)=GGs,Vs1=Vs2 
  ; select((B:-As),Vs1,Vs_),Vs2=[GGs|Vs_]
  ), % outer select loop
  select(A,As,Bs),         % inner select loop
  ljj_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim empty bodies
  ljj(G,[NewB|Vs2]).


ljj_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljj_imp((D:-Cs),B,Vs):- ljj((D:-Cs),[(B:-[D])|Vs]).

/*
ljj_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljj_imp((D:-Cs),B,Vs1):- 
  append(Cs,Vs1,Vs2),
  ljj(D,[(B:-[D])|Vs2]).
*/

% timed variants

timed_hprove(T):-timed_hprove(600,T).

timed_hprove(Max,T):-
  timed_call(Max,hprove(T),Time),
  (compound(Time)->ppp(Time:T);true).
  
iprove(T0):-toHorn(T0,T),lji(T).  

lji(A):-lji(A,[]),!.

lji(A,Vs):-memberchk(A,Vs),!. 
lji((B:-As),Vs1):-!,append(As,Vs1,Vs2),lji(B,Vs2).
lji(G,Vs1):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  lji_imp(A,B,Bs,NewB,Vs2), % A element of the body of B
  !,
  lji(G,[NewB|Vs2]).

lji_imp(A,B,[], B,Vs):-atomic(A),!,memberchk(A,Vs).
lji_imp(A,B,Bs, (B:-Bs),Vs):-atomic(A),!,memberchk(A,Vs).
lji_imp((D:-Cs),B,[], B,Vs):-!,lji((D:-Cs),[(B:-[D])|Vs]).
lji_imp((D:-Cs),B,Bs, (B:-Bs),Vs):-lji((D:-Cs),[(B:-[D])|Vs]).

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


xprove(T0):-toHorn(T0,T),ljy(T).

yprove(T0):-toSortedHorn(T0,T),ljy(T).

ljy(A):-ljy(A,[]),!.

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

zprove(T0):-toSortedHorn(T0,T),ljz(T,[]),!.

ljz(A):-ljz(A,[]),!.

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

hgprove(T0):-toHorn(T0,T),ljg(T).

ljg(A):-ljg(A,[]),!.

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

