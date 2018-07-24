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



h1prove(T0):-toHorn(T0,T),ljh1(T).

ljh1(A):-ljh1(A,[]),!.

%ljh1(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps
%ljh1(A,Vs):-memberchk(A,Vs),!. 
ljh1((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljh1(B,Vs2).
ljh1(G,Vs1):- % atomic(G), G not on Vs1
  ( memberchk(G,Vs1)->true
  ; memberchk((G:-_),Vs1),  
    select((B:-As),Vs1,Vs2), % outer select loop
    select(A,As,Bs),         % inner select loop
    ljh1_imp(A,B,Vs2), % A element of the body of B
    !,
    trimmed((B:-Bs),NewB), % trim empty bodies
    ljh1(G,[NewB|Vs2])
  ).
  
ljh1_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljh1_imp((D:-Cs),B,Vs):- ljh1((D:-Cs),[(B:-[D])|Vs]).

hhd((H:-_),(H:-_)).
hhd(H,H).






hqprove(T0):-toEqHorn(T0,T),ljhq(T).

ljhq(A):-ljhq(A,[]),!.

%ljhq(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps
ljhq(A,Vs):-memberchk(A,Vs),!. 
ljhq((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljhq(B,Vs2).
ljhq((A<->B),Vs):-!,ljhq(A,[B|Vs]),ljhq(B,[A|Vs]).
ljhq(G,Vs1):- % atomic(G), G not on Vs1
  %memberchk((G:-_),Vs1), % if not, we just fail
  sel_eq((B:-As),Vs1,Vs2), % outer select loop
  sel_eq(A,As,Bs),         % inner select loop
  ljhq_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim empty bodies
  ljhq(G,[NewB|Vs2]).
  
ljhq_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljhq_imp((D:-Cs),B,Vs):- ljhq((D:-Cs),[(B:-[D])|Vs]).

sel_eq(Y,Vs1,Vs3):-select(X,Vs1,Vs2),exp_eq(X,Y,Vs2,Vs3).

exp_eq((A<->B),X,Vs,[Y|Vs]):-!,pick_eq((A:-[B]),(B:-[A]),X,Y).
exp_eq(X,X,Vs,Vs).

pick_eq(A,B,A,B).
pick_eq(A,B,B,A).

% Horn + equivalences
  
haprove(T0):-toEqHorn(T0,T),ljha(T).

ljha(A):-ljha(A,[]),!.

%ljha(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps
ljha(A,Vs):-atomic(A),memberchk(A,Vs),!. 
ljha((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljha(B,Vs2).
ljha((A<->B),Vs):-!,ljha(A,[B|Vs]),ljha(B,[A|Vs]).
ljha(G,Vs1):-
  select(Red,Vs1,Vs2),
  ljha_reduce(Red,Vs2,Vs3),
  !,
  ljha(G,Vs3).

%ljha_reduce(Red,Vs,Vs):-ppp(reduce:(Vs-->Red)),fail.  
ljha_reduce((B:-As),Vs,[NewB|Vs]):-!,
  select(A0,As,Bs0),
  expand_eq(A0,Bs0,A,Bs), 
  ljha_imp(A,B,Vs),
  !,
  trimmed((B:-Bs),NewB).
ljha_reduce(A<->B,Vs,[(A:-[B]),(B:-[A])|Vs]).



expand_eq(X<->Y,Bs,(X:-[Y]),[(Y:-[X])|Bs]):-!.
expand_eq(A,Bs,A,Bs).

%ljha_imp(A,B,Vs):-ppp(imp:(Vs-->['A'=A,'B'=B])),fail.
ljha_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljha_imp((D:-Cs),B,Vs):-!,ljha((D:-Cs),[(B:-[D])|Vs]).

% more complex, tries to have only one pass - not worth it
hh1prove(T0):-toHorn(T0,T),hh1(T).

hh1(A):-hh1(A,[]),!.

%hh1(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps

hh1(G,Vs):-atomic(G),fine_atom_in(Vs,G,Atom,Impl),
  ( nonvar(Atom) -> !
  ;  var(Impl)->!,fail
  ;  fail
  ).
hh1((B:-As),Vs1):-!,append(As,Vs1,Vs2),hh1(B,Vs2).  
hh1(G,Vs1):- % atomic(G), G not on Vs1
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  hh1_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim empty bodies
  hh1(G,[NewB|Vs2]).
  
hh1_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
hh1_imp((D:-Cs),B,Vs):- hh1((D:-Cs),[(B:-[D])|Vs]).

fine_atom_in([],_,_Atom,_Impl):-!.
fine_atom_in([G|_],G,true,_):-!.
fine_atom_in([(G:-_)|Xs],G,Atom,true):-!,fine_atom_in(Xs,G,Atom,true).
fine_atom_in([_|Xs],G,Atom,Impl):-fine_atom_in(Xs,G,Atom,Impl).



% transforms to an equational form, then depth at most 3 Horn

wprove(A):-toFlatHorn(A,B),ljh(B).


% reduces eagely, possibly better for Horn3
% but tests do not confirm this
h3prove(T0):-toHorn(T0,T),ljh3(T).

w3prove(A):-toFlatHorn(A,B),ljh3(B).

ljh3(A):-ljh3(A,[]),!.


ljh3(A,Vs):-portray_clause(((A:-Vs))),nl,fail.
ljh3(A,Vs):-memberchk(A,Vs),!. 
ljh3((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljh3(B,Vs2).
ljh3(G,Vs1):-
  memberchk((G:-_),Vs1),
  ljh3_reduces(0,K,Vs1,Vs2_),K>0,sort(Vs2_,Vs2),
  %ljh3_reduce(Vs1,Vs2),
  %( K>5->ppp(K),ppp(Vs1),ppp(Vs2),nl;true),
  ljh3(G,Vs2).

ljh3_reduce(Vs1,_):-portray_clause((reduce:-Vs1)),nl,fail.  
ljh3_reduce(Vs1,[NewB|Vs2]):-
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  ljh3_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB). % trim empty bodies
 
ljh3_imp(A,B,Vs):-ppp(ljh3_imp(A,B,Vs)),nl,fail.  
ljh3_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljh3_imp((D:-Cs),B,Vs):- ljh3((D:-Cs),[(B:-[D])|Vs]).

% loops for reduce->reduces k

ljh3_reduces(K1,_,Vs1,_):-portray_clause((ljh3_reduces(K1):-Vs1)),nl,fail.
ljh3_reduces(K1,K3,Vs1,Vs3):- succ(K1,K2),
   ljh3_reduce(Vs1,Vs2),
   %assertion(Vs1\=Vs2),
   !,
   ljh3_reduces(K2,K3,Vs2,Vs3).
ljh3_reduces(K,K,Vs,Vs).

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

