% works on Horn clauses - includes
% preprocessing from implicational form
% from which the translation is reversible

hprove(T0):-toHorn(T0,T),ljh(T).

ljh(A):-ljh(A,[]).

%ljh(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps

ljh(A,Vs):-memberchk(A,Vs),!. 
ljh((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljh(B,Vs2).
ljh(G,Vs1):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  ljh_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim off empty bodies
  ljh(G,[NewB|Vs2]). 
  
ljh_imp((D:-Cs),B,Vs):- !,ljh((D:-Cs),[(B:-[D])|Vs]).
ljh_imp(A,_B,Vs):-memberchk(A,Vs).

trimmed((B:-[]),R):-!,R=B. 
trimmed(BBs,BBs).

hrprove(T0):-toHorn(T0,T),hrlj(T).

hrlj(A):-hrlj(A,[]).

hrlj(A,Vs):-memberchk(A,Vs),!. 
hrlj((B:-As),Vs1):-!,append(As,Vs1,Vs2),hrlj(B,Vs2).
hrlj(G,Vs1):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  hrlj_reduces(Vs1,Vs2),
  hrlj(G,Vs2). 
  
hrlj_reduce(Vs1,[NewB|Vs2]):-
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  hrlj_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB). % trim off empty bodies

hrlj_imp((D:-Cs),B,Vs):- !,hrlj((D:-Cs),[(B:-[D])|Vs]).
hrlj_imp(A,_B,Vs):-memberchk(A,Vs).

% hrlj_reduce changed into hrlj_reduces 
% is correct but not more efficient

hrlj_reduces-->hrlj_reduce,hrlj_reduces1. % at least one success

hrlj_reduces1-->hrlj_reduce,!,hrlj_reduces1.
hrlj_reduces1-->[].


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
  
ljg_imp((D:-Cs),B,Vs):-!,ljg((D:-Cs),[(B:-[D])|Vs]).
ljg_imp(A,_B,Vs):-memberchk(A,Vs).

% with ~A as A->false - only expands negation
hnprove(T0):-toNHorn(T0,T),ljnh(T).

toNHorn --> expand_neg,toHorn.

   
% nested Horn - from all except disjunction after Mints transform

hmprove(T0):-hmints(T0,T),
   % ppp(T0),ppp(T),nl,
   ichs(T).

% nested Horn - from all except disjunction
ichprove(T0):-toNestedHorn(T0,T),
   % ppp(T0),ppp(T),nl,
   ichs(T).

ichs(A):-ichs(A,[]).


%icljh(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps

ichs([],_):-!.
ichs([C|Cs],Vs):-!,ljnh(C,Vs),ichs(Cs,Vs).
ichs(X,Vs):-ljnh(X,Vs).

% nested Horn + false + true - compiled from all except disjunction

ljnh(A):-ljnh(A,[]).

%ljnh(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps
ljnh(true,_):-!.
ljnh(A,Vs):-memberchk(A,Vs),!. 
ljnh(_,Vs):-memberchk(false,Vs),!.
ljnh((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljnh(B,Vs2).
ljnh(G,Vs1):- % atomic(G), G not on Vs1
  membtest(G,Vs1),
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  ljnh_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim empty bodies
  ljnh(G,[NewB|Vs2]).
  
ljnh_imp((D:-Cs),B,Vs):-!,ljnh((D:-Cs),[(B:-[D])|Vs]).
ljnh_imp(true,_B,_Vs):-!.
ljnh_imp(A,_B,Vs):-memberchk(A,Vs).

%membtest(G,Vs):-ppp(membtest(G,Vs)),fail.
membtest(G,Vs):-memberchk((G:-_),Vs),!. % if not, we just fail
membtest(_,Vs):-memberchk((false:-_),Vs). % could still be infered from false


%%%%%%%%%%%%%%%%%%%%%%%

hvprove(T0):-toVarHorn(T0,T),ljhv(T).

ljhv(A):-ljhv(A,[]),!.

ljhv(A,Vs):-member_var(A,Vs),!. 
ljhv(G,Vs1):-nonvar(G),!,G=(B:-As),
  append(As,Vs1,Vs2),ljhv(B,Vs2).
ljhv(G,Vs1):- % var(G), G not on Vs1
  member((GG:-_),Vs1),GG==G,  % if not, we just fail
  !,
  select_nonvar((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  ljhv_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim empty bodies
  ljhv(G,[NewB|Vs2]).
  
ljhv_imp(A,_B,Vs):-var(A),!,member_var(A,Vs).
ljhv_imp((D:-Cs),B,Vs):- ljhv((D:-Cs),[(B:-[D])|Vs]).


member_var(V,Xs):-member(VV,Xs),VV==V,!.

select_nonvar(X,Xs,Ys):-select(Y,Xs,Ys),nonvar(Y),Y=X.

h1prove(T0):-toHorn(T0,T),ljh1(T).

ljh1(A):-ljh1(A,[]),!.

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


% reduces eagerly, possibly better for Horn3
% but tests do not confirm this
h3prove(T0):-toHorn(T0,T),ljh3(T).

w3prove(A):-toFlatHorn(A,B),ljh3(B).

ljh3(A):-ljh3(A,[]),!.


%ljh3(A,Vs):-portray_clause(((A:-Vs))),nl,fail.
ljh3(A,Vs):-memberchk(A,Vs),!. 
ljh3((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljh3(B,Vs2).
ljh3(G,Vs1):-
  memberchk((G:-_),Vs1),
  ljh3_reduces(0,K,Vs1,Vs2_),K>0,sort(Vs2_,Vs2),
  %ljh3_reduce(Vs1,Vs2),
  %( K>5->ppp(K),ppp(Vs1),ppp(Vs2),nl;true),
  ljh3(G,Vs2).

%ljh3_reduce(Vs1,_):-portray_clause((reduce:-Vs1)),nl,fail.  
ljh3_reduce(Vs1,[NewB|Vs2]):-
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  ljh3_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB). % trim empty bodies
 
%ljh3_imp(A,B,Vs):-ppp(ljh3_imp(A,B,Vs)),nl,fail.  
ljh3_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljh3_imp((D:-Cs),B,Vs):- ljh3((D:-Cs),[(B:-[D])|Vs]).

% loops for reduce->reduces k

%ljh3_reduces(K1,_,Vs1,_):-portray_clause((ljh3_reduces(K1):-Vs1)),nl,fail.
ljh3_reduces(K1,K3,Vs1,Vs3):- succ(K1,K2),
   ljh3_reduce(Vs1,Vs2),
   %assertion(Vs1\=Vs2),
   !,
   ljh3_reduces(K2,K3,Vs2,Vs3).
ljh3_reduces(K,K,Vs,Vs).

% seeing  hprove as working on Horn clauses with compound heads

hhprove(A):-toHorn(A,H),hlj(H),!.

hlj1(H):-hlj(H),!.

hlj((A:-Vs)):-memberchk(A,Vs),!. 
hlj(((B:-As):-Vs1)):-!,append(As,Vs1,Vs2),hlj(B:-Vs2).
hlj((G:-Vs1)):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  hlj_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim empty bodies
  hlj((G:-[NewB|Vs2])).
  

hlj_imp((D:-Cs),B,Vs):-!, hlj((D:-Cs):-[(B:-[D])|Vs]).
hlj_imp(A,_B,Vs):- memberchk(A,Vs).

%%%%%%%%

:-op(800,xfx,(<-)).

nhprove(A):-toAHorn(A,H),nhlj(H).

nhlj(A<-Vs):-memberchk(A,Vs),!. 
nhlj((B<-As)<-Vs1):-!,append(As,Vs1,Vs2),nhlj(B<-Vs2).
nhlj(G<-Vs1):- % atomic(G), G not on Vs1
  memberchk(G<-_,Vs1), % if not, we just fail
  select(B<-As,Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  nhlj_imp(A,B,Vs2), % A element of the body of B
  !,
  atrimmed(B<-Bs,NewB), % trim empty bodies
  nhlj(G<-[NewB|Vs2]).
  
nhlj_imp(D<-Cs,B,Vs):-!,nhlj((D<-Cs)<-[(B<-[D])|Vs]).
nhlj_imp(A,_B,Vs):-memberchk(A,Vs).

atrimmed(B<-[],R):-!,R=B. 
atrimmed(BBs,BBs).

% faster so far
ahprove(A):-toAHorn(A,H),call(H).

A<-Vs:-memberchk(A,Vs),!. 
(B<-As)<-Vs1:-!,append(As,Vs1,Vs2),B<-Vs2.

G<-Vs1:- % atomic(G), G not on Vs1
  memberchk((G<-_),Vs1), % if not, we just fail
  select(B<-As,Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  ahlj_imp(A,B,Vs2), % A element of the body of B
  !,
  atrimmed(B<-Bs,NewB), % trim empty bodies
  G<-[NewB|Vs2].
  
ahlj_imp(D<-Cs,B,Vs):-!, (D<-Cs)<-[B<-[D]|Vs].
ahlj_imp(A,_B,Vs):- memberchk(A,Vs).


/*
% alternative ahprove: definitely slower !!!
% it seems that forcing the clean-up of reversible rules first
% does not help with performance, at least on terms up to size 15
ahprove(A):-toAHorn(A,H),call(H).

A<-Vs:-memberchk(A,Vs),!. 
(B<-As)<-Vs1:-!,append(As,Vs1,Vs2),B<-Vs2.

G<-Vs1:- % atomic(G), G not on Vs1
  memberchk((G<-_),Vs1), % if not, we just fail
  hreduce(BBs,Vs1,Vs2),
  !,
  atrimmed(BBs,NewB), % trim empty bodies  
  G<-[NewB|Vs2].

hreduce(B<-Bs,Vs1,Vs2):-
  select(B<-As,Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  memberchk(A,Vs2),
  !.
hreduce(B<-Bs,Vs1,Vs2):-
  select(B<-As,Vs1,Vs2),   % outer select loop
  select(A,As,Bs),         % inner select loop
  ahlj_imp(A,B,Vs2).       % A element of the body of B
  
ahlj_imp(D<-Cs,B,Vs):-(D<-Cs)<-[B<-[D]|Vs].
%ahlj_imp(A,_B,Vs):- memberchk(A,Vs).
*/


%%%%%%%% 

hhhprove(A):-toHorn(A,H),hhlj(H),!.

hhlj1(H):-hhlj(H),!.

hhlj((A:-Vs)):-memberchk(A,Vs),!. 
hhlj(((B:-As):-Vs1)):-!,append(As,Vs1,Vs2),hhlj(B:-Vs2).
hhlj((G:-Vs1)):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  select((B:-As),Vs1,Vs2), % outer select loop
  hhlj_sel((B:-As),Bs,Vs2),
  !,
  trimmed((B:-Bs),NewB), % trim empty bodies
  hhlj((G:-[NewB|Vs2])).

hhlj_sel((_:-As),Bs,Vs):-  
  select(A,As,Bs),         % inner select loop
  %atomic(A),
  memberchk(A,Vs).
hhlj_sel((B:-As),Bs,Vs):-  
  select((D:-Cs),As,Bs),         % inner select loop
  hhlj((D:-Cs):-[(B:-[D])|Vs]).

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
  
ljo_imp((D:-Cs),B,Vs):-!,ljo((D:-Cs),[(B:-[D])|Vs]).
ljo_imp(A,_B,Vs):-memberchk(A,Vs).

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

lji(A):-lji(A,[]).

lji(A,Vs):-memberchk(A,Vs),!. 
lji((B:-As),Vs1):-!,append(As,Vs1,Vs2),lji(B,Vs2).
lji(G,Vs1):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  lji_imp(A,B,Bs,NewB,Vs2), % A element of the body of B
  !,
  lji(G,[NewB|Vs2]).

  
lji_imp((D:-Cs),B,[], B,Vs):-!,lji((D:-Cs),[(B:-[D])|Vs]).
lji_imp((D:-Cs),B,Bs, (B:-Bs),Vs):-!,lji((D:-Cs),[(B:-[D])|Vs]).
lji_imp(A,B,[], B,Vs):-!,memberchk(A,Vs).
lji_imp(A,B,Bs, (B:-Bs),Vs):-memberchk(A,Vs).



iiprove(T0):-toHorn(T0,T),ljii(T).  

ljii(A):-ljii(A,[]).

ljii(A,Vs):-memberchk(A,Vs),!. 
ljii((B:-As),Vs1):-!,append(As,Vs1,Vs2),lji(B,Vs2).
ljii(G,Vs1):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  lji_imp2(Bs,A,B,NewB,Vs2), % A element of the body of B
  !,
  ljii(G,[NewB|Vs2]).

lji_imp2([],A,B,B,Vs):-!,lji_imp1(A,B,Vs).
lji_imp2(Bs,A,B,(B:-Bs),Vs):-lji_imp1(A,B,Vs).

lji_imp1((D:-Cs),B,Vs):-!,ljii((D:-Cs),[(B:-[D])|Vs]).
lji_imp1(A,_,Vs):-memberchk(A,Vs).


fprove(T0):-toListHorn(T0,T),ljf(T,[]).

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
  

ljf_imp([D|Cs],B,Vs):-!,ljf([D|Cs],[[B,D]|Vs]).
ljf_imp(A,_B,Vs):-memberchk(A,Vs).

ftrimmed([B],R):-!,R=B.
ftrimmed(BBs,BBs).


% fastest on bm/2, at this point
vprove(T0):-toListHorn(T0,T),ljv(T,[]),!.

ljv(A,Vs):-memberchk(A,Vs),!. 
ljv([B|As],Vs1):-!,append(As,Vs1,Vs2),ljv(B,Vs2).
ljv(G,Vs1):-
  memberchk([G|_],Vs1),
  ljv_choice(G,Vs1,End,End).

ljv_choice(G,[[B|As]|End],Vs2,End):-
  select(A,As,Bs),
  ljv_imp(A,B,Vs2),
  !,
  vtrimmed(Bs,B,G,Vs2).
ljv_choice(G,[Ys|Vs1],Vs2,End):-
  ljv_choice(G,Vs1,[Ys|Vs2],End).

ljv_imp([D|Cs],B,Vs):-!,ljv([D|Cs],[[B,D]|Vs]).
ljv_imp(A,_B,Vs):-memberchk(A,Vs).

vtrimmed([],B,G,Vs):-ljv(G,[B|Vs]).
vtrimmed([BB|Bs],B,G,Vs):-ljv(G,[[B,BB|Bs]|Vs]).



% works on Horn clauses - includes
% preprocessing from implicational form
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

  
