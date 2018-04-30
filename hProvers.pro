% provers using embedded Horn Clauses or customized
% for also handling negation and classic proofs
% via Glivenko's translation from classical to intuitionistic
% propositional calculus

:- op(425,  fy,  ~ ). % negation

% classicall logic propositional prover
% using Glivenko's double negation translation

% supports also negation seen as A->false
clprove(T0):-dneg(T0,T),cprove(T).

cprove(T0):-
 must_be(ground,T0),
 expand_neg(T0,T),
 ljc(T,[]),
 !.

ljc(_,Vs):-memberchk(false,Vs),!.
ljc(A,Vs):-memberchk(A,Vs),!.
ljc((A->B),Vs1):-!,add_new(A,Vs1,Vs2),ljc(B,Vs2). 
ljc(G,Vs1):- % atomic(G),
  select((A->B),Vs1,Vs2),
  ljc_imp(A,B,Vs2),
  !,
  add_new(B,Vs2,Vs3),
  ljc(G,Vs3).

ljc_imp(A,_,Vs):-integer(A),!,memberchk(A,Vs).   
ljc_imp((C->D),B,Vs1):- 
   add_new((D->B),Vs1,Vs2),
   ljc((C->D),Vs2).

expand_neg(A,R):-atomic(A),!,R=A.
expand_neg(~A,R):-!,expand_neg(A,B),R=(B->false).
expand_neg((A->B),(X->Y)):-expand_neg(A,X),expand_neg(B,Y).


% assumes all hypotheses at once
% while avoiding duplicates

hprove(T):-ljh(T,[]),!.

ljh(A,Vs):-memberchk(A,Vs),!.
ljh(As,Vs1):-As=(_->_),!,
  assume_all(As,B,Vs1,Vs2),
  ljh(B,Vs2). 
ljh(G,Vs1):- % atomic(G),
  select((A->B),Vs1,Vs2),
  ljh_imp(A,B,Vs2),
  !,
  add_new(B,Vs2,Vs3),
  ljh(G,Vs3).

ljh_imp(A,_,Vs):-atomic(A),!,memberchk(A,Vs).   
ljh_imp((C->D),B,Vs1):-
   add_new((D->B),Vs1,Vs2),
   ljh((C->D),Vs2).
  
assume_all(A,Last,As,As):-atomic(A),!,Last=A.
assume_all(A->B,Last,As,Bs):-
   memberchk(A,As),
   !,
   assume_all(B,Last,As,Bs).
assume_all(A->B,Last,As,[A|Bs]):-
   assume_all(B,Last,As,Bs).

% works on Horn clauses - includes
% preporcessing from implicational form
% from which the translation is reversible except for order

xprove(T0):-toHorn(T0,T),ljx(T,[]),!.

xprove_init(T0):-toHorn(T0,_).

ljx(A,Vs):-memberchk(A,Vs),!. 
ljx((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljx(B,Vs2).
ljx(G,Vs1):- % atomic(G), G not on Vs
  select((B:-As),Vs1,Vs2),
  select(A,As,Bs), 
  ljx_imp(A,B,Vs2),
  !,
  trimmed((B:-Bs),NewB),
  ljx(G,[NewB|Vs2]).
  
ljx_imp(A,_B,Vs):-atomic(A),!,memberchk(A,Vs).
ljx_imp((D:-Cs),B,Vs):- ljx((D:-Cs),[(B:-[D])|Vs]).

trimmed((A:-[]),A).
trimmed((A:-[B|Bs]),A:-[B|Bs]).


% works on Horn clauses - includes
% preporcessing from implicational form
% from which the translation is reversible except for order

yprove(T0):-toSortedHorn(T0,T),ljy(T,[]),!.

yprove_init(T0):-toSortedHorn(T0,_).

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

add_all([],Ys,Ys):-!.
add_all([X|Xs],Ys,Rs):-
   memberchk(X,Ys),
   !,
   add_all(Xs,Ys,Rs).
add_all([X|Xs],Ys,[X|Rs]):-
add_all(Xs,Ys,Rs).




% variant of xprove, with nondeterministic part
% confined to zreduce/2

zprove0(T0):-toSortedHorn(T0,_). % baseline for benchmarks

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

% trims implications when statically equivalent to A->A
qprove(T0):-
  trimImps(T0,T),
  %ppp(T),
  ljq(T,[]),!.

qprove0(T0):-trimImps(T0,_). % for benchmarking baseline
  
ljq(A,Vs):-memberchk(A,Vs),!.
ljq((A->B),Vs1):-!,add_new(A,Vs1,Vs2),ljq(B,Vs2). 
ljq(G,Vs1):- % atomic(G),
  select(As,Vs1,Vs2),
  imphead(As,B),
  impsel(A,As,Bs),
  ljq_imp(A,B,Vs2),
  !,
  add_new(Bs,Vs2,Vs3),
  ljq(G,Vs3).

ljq_imp(A,_,Vs):-atomic(A),!,memberchk(A,Vs).   
ljq_imp((C->D),B,Vs1):-    
    add_new((D->B),Vs1,Vs2),
    add_new(C,Vs2,Vs3),
    ljq(D,Vs3).

imphead(_->As,H):-!,imphead(As,H).
imphead(H,H).

impsel(A,(A->Bs),Bs).
impsel(A,(B->Bs),(B->Cs)):-impsel(A,Bs,Cs).  
