:-op(600,xfx,(<-)).

% prover for nested Horn, compiled from all except disjunction

holds(A):-A<-[].

A<-Vs:-memberchk(A,Vs),!.
(B<-As)<-Vs1:-!,append(As,Vs1,Vs2),B<-Vs2.
G<-Vs1:- % atomic(G), G not on Vs1
  memberchk((G<-_),Vs1),
  select((B<-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  holds_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B<-Bs),NewB), % trim empty bodies
  G<-[NewB|Vs2].
  
holds_imp((D<-Cs),B,Vs):-!,(D<-Cs)<-[(B<-[D])|Vs].
holds_imp(A,_B,Vs):-memberchk(A,Vs).

trimmed((B<-[]),R):-!,R=B.
trimmed(BBs,BBs).



%%%%%%%%%

simple(X):-atomic(X),!.
simple(X):-var(X).

nvar(_,Xs,Xs).

hcan(A,R):-simple(A),!,R=A.
hcan((H<-Bs),(H<-Ds)):-hcans(0,Bs,Cs,Ds,Cs).

hcans(_,[],[])-->[].
hcans(K,[B|Bs],[C|Cs])-->
  heq(K,B,C),
  hcans(K,Bs,Cs).

heq(_,A,R)-->{simple(A)},!,{R=A}.
heq(K,(H<-Bs),R)-->
   {K1 is K+1},
   hcans(K1,Bs,Cs),
   maybe_shallow(K,H,Cs,R).

maybe_shallow(K,H,Cs,R)-->
  {T=(H<-Cs),depth(T,D),K+D<3},!,{R=T}.
maybe_shallow(_K,H,Cs,N)-->
   nvar(N),
   [(N<-(H<-Cs)),(H<-[N|Cs])].

depth(A,0):-simple(A),!.
depth(_<-Bs,D):-
  maplist(depth,Bs,Ds),
  max_list(Ds,D1),
  D is 1+D1.


ppp(H<-Bs):-!,portray_clause((H:-Bs)).
ppp(X):-portray_clause(X).

ht(T):-
  ppp('PROBLEM:'),
  ppp(T),
  hcan(T,C),
  nl,ppp('ANSWER:'),
  ppp(C),
  nl,ppp('END!'),nl,
  fail.


cht1:-
  T=f<-[a,g<-[b<-[c,d<-[e,q],x],m],r<-[p,x]],
  ht(T).


cht2:-
  T=f<-[a,g<-[b<-[c,d,x],m],r<-[p,x]],
  ht(T).


cht3:-
  T=f<-[a,g<-[b<-[c,d<-[e<-[i,j],f<-[k]]],m],r<-[p,x]],
  ht(T).


cht4:-
  T= 0<-[0<-[0<-[0<-[0]]]],
  ht(T).

chgo:-cht1;cht2;cht3;cht4.



go:-
 T=x<-[g<-[g,h]],
 holds(T).

