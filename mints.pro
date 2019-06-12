% variant of the transformation from Mints 92, 
% reducing formulas to a canonical form
% here we also handle <-> and reduce ~p to p -> false
mints(E,T,Rs):-mints0(E,T,Rs,[]),!.
mints(E,T,Rs):-
  reset_gensym('nv'),
  mints(E,T,Es,[]),
  !,
  reverse(Es,Xs),
  %remdup(Xs,Rs),
  %revsort(Xs,Rs),
  sort(Xs,Rs),
  %ppp(T:Rs),
  true.

%mints0(~X,false)-->[X],{primitive(X)},!.
mints0(~B,~B)-->{primitive(B)},!.
mints0((X->Y),Y)-->[X],{primitive(X),primitive(Y)},!.
mints0((X->(Y->Z)),Z)-->[X,Y],{primitive(X),primitive(Y),primitive(Z)},!.
mints0(((X->Y)->Z),Z)-->[X->Y],{primitive(X),primitive(Y),primitive(Z)},!.
mints0(((X&Y)->Z),Z)-->[X,Y],{primitive(X),primitive(Y),primitive(Z)},!.

%mints(~A,R)-->mints((A->false),R).
mints(~B,A)-->!,nv(A),mints(B,B1),[(A-> ~B1),(~B1->A)].
mints((B->C),A)-->nv(A),mints(B,B1),mints(C,C1),[A->(B1->C1),(B1->C1)->A].
mints(B&C,A)-->nv(A),mints(B,B1),mints(C,C1),[A->B1,A->C1,B1->(C1->A)].
mints(B<->C,A)-->mints(B,B1),mints(C,C1),mints((B1->C1) & (C1->B1),A).
mints(B v C,A)-->nv(A),mints(B,B1),mints(C,C1),[B1->A,C1->A,A->(B1 v C1)].
mints(P,A)-->{primitive(P)},{A=P}.

nv(X,Es,Es):-gensym('nv',X).

mints(T,MT):-
  mints(T,H,Bs),
  unexpand(Bs,H,MT).
  
% turns Vs,G into V1-&V2->...->G 


list2conj([],G,G).
list2conj([V|Vs],G,(V&R)):-list2conj(Vs,G,R).
