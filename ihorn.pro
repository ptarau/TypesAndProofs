:-include('allPartitions.pro').
:-include('stats.pro').

:-op(600,xfx,<-).
:-op(600,xfx,<=).
/*
holds(A):-A<-[].

G<-Vs:-memberchk(G,Vs),!.
%(B<-As)<-Vs1:-!,append(As,Vs1,Vs2),B<-Vs2. % call
G<-Vs1:- % atomic(G), G not on Vs1
  memberchk(G<-_,Vs1),
  select(B<-As,Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  %holds_imp(A,B,Vs2), % A element of the body of B
  memberchk(A,Vs2),
  !,
  trimmed(B<-Bs,NewB), % trim empty bodies
  G<-[NewB|Vs2]. % call
  
%holds_imp((D<-Cs),B,Vs):-!,(D<-Cs)<-[(B<-[D])|Vs].
holds_imp(A,_B,Vs):-memberchk(A,Vs).

trimmed((B<-[]),R):-!,R=B.
trimmed(BBs,BBs).
*/

G<-Vs:-
   memberchk(G<-_,Vs),
   G<=Vs.

G<=Vs:-memberchk(G<-[],Vs),!.
G<=Vs1:- % atomic(G), G not on Vs1
  select(B<-As,Vs1,Vs2), % outer select loop
  select(A,As,Bs),       % inner select loop
  memberchk(A<-[],Vs2),
  !,
  G<=[B<-Bs|Vs2]. % call




horns([],_,_).
horns([G|Gs],Cs,Ts):-
   not(memberchk(G,Ts)),
   member(G<-Bs,Cs),
   append(Bs,Gs,NewGs),
   horns(NewGs,Cs,[G|Ts]).

horn(H<-Bs):-horns([H],Bs,[]).



htheo(N,T):-all_ihorns(N,T),once(horn(T)).

% generator

hgen(N,T):-numlist(0,N,Vs),hgen(T,Vs,[]).

e(X,[X|Xs],Xs).

bgen([])-->[].
bgen([B|Xs])-->e(B),bgen(Xs).

cgen([])-->[].
cgen([C|Cs])-->e(H),bgen(Bs),cgen(Cs), {C=(H<-Bs)}. %,  {trimmed(H<-Bs,C)}.

hgen(H<-Bs)-->e(H),cgen(Bs).

all_ihorns(N,T):-
   length(Vs,N),
   hgen(T,Vs,[]),
   natpartitions(Vs,_).

itheo(N,T):-all_ihorns(N,T),once(T).

tcount(N,C):-sols(itheo(N,_),C).

igo:-all_ihorns(6,T),portray_clause(T),T,writeln('!'),fail.

hcount(N,C):-sols(htheo(N,_),C).

tcounts:-
  M=9,
  numlist(1,M,Ns),
  maplist(tcount,Ns,Cs),
  writeln(Cs).

hcounts:-
  M=9,
  numlist(1,M,Ns),
  maplist(hcount,Ns,Cs),
  writeln(Cs).


bug(N,T,R):-
  all_ihorns(N,T),
  ( once(T),not(horn(T)),R=(t>h)
  ; once(horn(T)),not(T),R=(h>t)
  ).


bug1:- T=0<-[0<-[1, 2], 1<-[2], 2<-[]],horn(T).
