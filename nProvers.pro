handprove(T0):-
  %nl,ppp(T0),
  expand_equiv(T0,T1), 
  %ppp(T1),
  toAndHorn(T1,T),
  %ppp(T),
  ljand(T).

ljand(A):-ljand(A,[]).

%ljand(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps

ljand(_,Vs):-memberchk(false,Vs),!.
ljand([],_):-!.
ljand([X|Xs],Vs):-!,ljand(X,Vs),ljand(Xs,Vs).
ljand(true,_):-!.
ljand(A,Vs):-memberchk(A,Vs),!. 
ljand((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljand(B,Vs2).

ljand(G,Vs0):- % atomic(G), G not on Vs1
  fix_heads(Vs0,Vs1),
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  ljand_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim off empty bodies
  ljand(G,[NewB|Vs2]). 
  
ljand_imp(([]:-_),_,_):-!.
ljand_imp((D:-Cs),B,Vs):- !,ljand((D:-Cs),[(B:-[D])|Vs]).
ljand_imp(true,_B,_Vs):-!.
ljand_imp(A,_B,Vs):-memberchk(A,Vs).

fix_head(([H|Hs]:-Bs),HBss):-!,distribute_head([H|Hs],Bs,HBss).
fix_head(X,[X]).

%fix_heads(Xs,Ys):-maplist(fix_head,Xs,Yss),flatten_it(Yss,Ys).

fix_heads(Xs,Ys):-maplist(fix_head,Xs,Yss),append(Yss,Ys).


ljxprove(T0):-
  %nl,ppp(T0),
  expand_equiv(T0,T1), 
  %ppp(T1),
  toAndHorn(T1,T),
  %ppp(T),
  ljx(T).
  
  
ljx(A):-ljx(A,[]).

%ljx(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps

ljx(_,Vs):-memberchk(false,Vs),!.
ljx([],_):-!.
ljx([X|Xs],Vs):-!,ljx(X,Vs),ljx(Xs,Vs).
ljx(true,_):-!.
ljx(A,Vs):-memberchk(A,Vs),!. 
%ljx((Bs:-As),Vs):-is_list(Bs),!,fix_head((Bs:-As),Xss),ljx(Xss,Vs).
ljx((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljx(B,Vs2).
ljx(G,Vs0):- % atomic(G), G not on Vs1
  fix_heads(Vs0,Vs0_),sort(Vs0_,Vs1),
  select((B:-As),Vs1,Vs2),
  select((B:-As),Vs1,Vs2),
  select(A,As,Bs),         % inner select loop
  ljx_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim off empty bodies
  ljx(G,[NewB|Vs2]). 
 
ljxs([],_).
ljxs([X|Xs],Vs):-ljx(X,Vs),ljxs(Xs,Vs).

ljx_imp(([]:-_),_,_):-!.
ljx_imp((D:-Cs),B,Vs):- !,ljx((D:-Cs),[(B:-[D])|Vs]).
ljx_imp(true,_B,_Vs):-!.
ljx_imp(A,_B,Vs):-memberchk(A,Vs).


selfix((H:-Bs),Vs1,Vs3):-
  select((Hx:-Bs),Vs1,Vs2),
  fix_head((Hx:-Bs),Xss),
  select((H:-Bs),Xss,Yss),
  append(Yss,Vs2,Vs3).


nhornprove(T0):-toNestedHorn(T0,T),
   %ppp(T0),ppp(T),nl,
   nhprs(T).

nhprs(A):-nhprs(A,[]),!.


%icljh(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps

nhprs([],_):-!.
nhprs([C|Cs],Vs):-!,nhorn(C,Vs),nhprs(Cs,Vs).
nhprs(X,Vs):-nhorn(X,Vs).

% nested Horn + false + true - compiled from all except disjunction

nhorn(A):-nhorn(A,[]).

%nhorn(A,Vs):-ppp((Vs-->A)),fail. % just to trace steps
nhorn(true,_).
nhorn(A,Vs):-A\==true,member(A,Vs). 
nhorn(G,Vs):- G\==false,member(false,Vs).
nhorn((B:-As),Vs1):-append(As,Vs1,Vs2),nhorn(B,Vs2).
nhorn(G,Vs1):- % primitive(G), % atomic(G), G not on Vs1
  \+ \+ membtest(G,Vs1),
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  nhorn_imp(A,B,Vs2), % A element of the body of B
  trimmed((B:-Bs),NewB), % trim empty bodies
  nhorn(G,[NewB|Vs2]).
 
%nhorn_imp(G,_B,_Vs):-ppp(nhorn_imp(G,B)),fail.

nhorn_imp(true,_B,_Vs).
nhorn_imp(A,_B,Vs):-A\==true,member(A,Vs).
nhorn_imp((D:-Cs),B,Vs):-nhorn((D:-Cs),[(B:-[D])|Vs]).
  

hbug:-nhornprove(
  ~ (0<->(0<-> ~0))
  ).

% N-Prolog's algorithm - no loop checking seems to pass our tests

npro(G):-
  %ppp(G),
  toHorn(G,H),npro(H,[],[]),!.

npro([],_,_):-!.
npro([X|Xs],Vs,Hs):-!,npro(X,Vs,Hs),npro(Xs,Vs,Hs).
npro((H:-Bs),Vs,Hs):-!,
  append(Bs,Vs,NewVs),
  npro(H,NewVs,Hs).
npro(G,Vs,_):-memberchk(G,Vs),!.
npro(G,Vs,Hs):- \+ memberchk(G,Hs),!, %ppp(G+Vs+Hs),
  member((G:-Bs),Vs), 
  npro(Bs,Vs,[G|Hs]).
npro(G,Vs1,Hs):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  ljh_imp_(A,B,Vs2,Hs), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim off empty bodies
  npro(G,[NewB|Vs2],Hs). 
  
ljh_imp_((D:-Cs),B,Vs,Hs):- !,npro((D:-Cs),[(B:-[D])|Vs],Hs).
ljh_imp_(A,_B,Vs,_):-memberchk(A,Vs).


nproloop:-
  G=(((0->1)->1)->1),
  npro(G).

npromiss:-
  G=((0->1)->((0->1)->0)->1),
  npro(G).

% full prover, after Mints' transform
redprove(G):-  
 expand_full_neg(G,F),
 mints(F,H,Bs),
 ljfa(H,Bs).
 
 
% all but disj, after Mints' transformation to canonical form  
fredprove(G):-  
 expand_full_neg(G,F),
 mints(F,H,Bs),
 ljfred(H,Bs).

%ljfred(G,Vs):-spy('ljf',G,Vs),fail.
ljfred(A,Vs):-memberchk(A,Vs),!.
ljfred(_,Vs):-memberchk(false,Vs),!.
ljfred((A->B),Vs):-!,ljfred(B,[A|Vs]).
ljfred(G,Vs1):- % atomic or false
  %rmembtest(G,Vs1),
  select((A->B),Vs1,Vs2),
  ljfred_imp(A,B,Vs2,Vs3),
  !,
  ljfred(G,Vs3).
  
%ljfred_imp(CD,_,_,_):-ppp(imp:CD),fail.
ljfred_imp((C->D),B,Vs,[B|Vs]):-!,ljfred((C->D),[(D->B)|Vs]).
ljfred_imp(A,B,Vs,[B|Vs]):-memberchk(A,Vs).  % atomic
  
rmembtest(G,Vs):-memberchk((_ -> G),Vs),!. % if not, we just fail
rmembtest(G,Vs):-memberchk((_ -> (_ ->G)),Vs),!. % if not, we just fail
rmembtest(_,Vs):-memberchk((_->false),Vs),!. % could still be infered from false
rmembtest(_,Vs):-memberchk((_ -> (_ ->false)),Vs),!. % if not, we just fail 

% all but disj, after Mints' transformation to canonical form  
gredprove(G):-  
 expand_full_neg(G,F),
 mints(F,H,Bs),
 ljgred(H,Bs).

%ljgred(G,Vs):-spy('ljf',G,Vs),fail.
ljgred(A,Vs):-memberchk(A,Vs),!.
ljgred(_,Vs):-memberchk(false,Vs),!.
ljgred((A->B),Vs):-!,ljgred(B,[A|Vs]).
ljgred(G,Vs1):- % atomic or false
  rmembtest(G,Vs1),
  prioritize(Red,A,B),
  select(Red,Vs1,Vs2),
  primitive(A),primitive(B),
  % select((A->B),Vs1,Vs2),
  ljgred_imp(Red,Vs2,Vs3),
  !,
  ljgred(G,Vs3).

prioritize(R,A,B):-
  (
    R=(A->B)
  ; R=(A->(_->B))
  ; R=((A->_)->B)
  ).
  
  
%ljgred_imp(CD,_,_,_):-ppp(imp:CD),fail.
ljgred_imp((C->D)->B,Vs,[B|Vs]):-!,ljgred((C->D),[(D->B)|Vs]).
ljgred_imp(A->B,Vs,[B|Vs]):-memberchk(A,Vs).  % atomic
  

xfprove(T0):-xljf(T0),!.

xljf(T0):-
  %ppp(T0),
  expand_full_neg(T0,T),
  %ppp(T),
  xljf(T,[]).

%xljf(G,Vs):-ppp('xljf'(G,Vs)),fail.
xljf(_,Vs):-memberchk(false,Vs),!.
xljf(?A,Vs):-member(?A,Vs).
xljf(A<->B,Vs):-!,xljf(B,[A|Vs]),xljf(A,[B|Vs]).
xljf((A->B),Vs):-!,xljf(B,[A|Vs]).
xljf(A & B,Vs):-!,xljf(A,Vs),xljf(B,Vs).
xljf(A v B, Vs):-(xljf(A,Vs);xljf(B,Vs)).
xljf(G,Vs1):-is_head(G),
  select(Red,Vs1,Vs2),
  xljf_reduce(Red,G,Vs2,Vs3),
  !,
  xljf(G,Vs3).

is_head(? _).
is_head(false).
is_head(_ v _).

%xljf_reduce(AB,G,Vs,Vs):-compound(AB),ppp(reduce(G):(AB:-Vs)),fail.
xljf_reduce((A->B),_,Vs1,Vs2):-xljf_imp(A,B,Vs1,Vs2).
xljf_reduce((A & B),_,Vs,[A,B|Vs]).
xljf_reduce((A<->B),_,Vs,[(A->B),(B->A)|Vs]).
xljf_reduce((A v B),G,Vs,[B|Vs]):-xljf(G,[A|Vs]).
  
%xljf_imp(CD,_,_,_):-ppp(imp:CD),fail.
xljf_imp(?A,B,Vs,[B|Vs]):-memberchk(?A,Vs).  
xljf_imp((C->D),B,Vs,[B|Vs]):-xljf((C->D),[(D->B)|Vs]).
xljf_imp((C & D),B,Vs,[(C->(D->B))|Vs]).
xljf_imp((C v D),B,Vs,[(C->B),(D->B)|Vs]).
xljf_imp((C<->D),B,Vs,[((C->D)->((D->C)->B))|Vs]).

% no bug anymore
xbug:-
  %T=((?0<->(?0<-> ?0)->false)->false),
  %T=(?0<->(?1<->(?0<->(?1<-> ?0)))),
  T=(?0<->(?0<->(?1<-> ?2))->false),
  xljf(T,[]).

% end
%xljf(?0,[?0,?1,?0,?1,?1,?0])
%xljf(?0,[?1,?0,?0,?1,?1,?0])
%xljf(?0,[?0,?0,?1,?1,?1,?0])
%xljf(?0,[?0,?1,?0,?1,?1,?0]) <---


 
 