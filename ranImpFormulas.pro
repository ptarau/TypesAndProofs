% random implicational logic formulas
% relies on ranPartition and RemyR (random binary trees)

ranImpFormula(N,T):-ranImpFormula(random,N,T).

ranHornFormula(N,T):-ranImpFormula(N,I),toHorn(I,T).

ranImpFormula(Seed,N,T):-
  set_random(seed(Seed)),
  N1 is N+1,
  ranSetPart(N1,Vs),
  remy(N,T,Vs).

ranImpFormulas(N,K,T):-ranImpFormulas(random,N,K,T).

% variant neede for gold standard tester
genRanImpFormulas(K,N,T):-ranImpFormulas(N,K,T).

ranImpFormulas(Seed,N,K,T):-
  Count is round(sqrt(K)),
  ranImpFormulas(Seed,N,Count,Count,T).
  
ranImpFormulas(Seed,N,PartCount,TreeCount,T):-
  set_random(seed(Seed)),
  N1 is N+1,
  bell(N1,Bell),
  between(1,TreeCount,_),
  remy(N,T,Vs),
  between(1,PartCount,_),
  ranPart(N1,Bell,Vs).
 
ranTypedImp(MaxSeed,M,N,T):-
  between(1,MaxSeed,Seed),
  ranImpFormula(Seed,M,T),
  hprove(T),
  sprove(T,X),
  type_of(X,TT),
  tsize(TT,Size),
  Size>N.
  
bigTypedImp(T):-ranTypedImp(20000,36,16,T).  

bigtest:-do((bigTypedImp(T),hprove(T),ppp(T))).


ranShow:-MSeed=1000,M=7,
 do((
   between(0,MSeed,Seed),
   ranImpFormula(Seed,M,T0),
   sprove1(T0,X),lsize(X,S),S>=M,
   varvars(T0,T),
   ppp(T),
   ppp(X),
   nl,
   ppt(X:T),
   qqq(X:T),
   nl,
   ppp('-------------')
 )).
  

/*

Can we generate a formula (that has a proof!) that is so hard that we cannot prove it?

*/



ranX(N,M,T):-
  repeat,
    remy(N,X),
    xtype_of(X,T),
    tsize(T,S),
    S>=M,
  !,
  natvars(T).

  
xtype_of(x,T):-xType(T).
xtype_of((A*B),T):-
  xtype_of(A,ST),
  xtype_of(B,S),
  unify_with_occurs_check(ST,(S->T)). 
  
  
/*
?- ranX(100,80,T),ahprove(T),ppp(T),fail.
(((((0->1->2)->(0->1)->0->2)->(3->4->3)->(0->1->2)->(0->1)->0->2)->(((5->6->5)->((7->8->9)->(7->8)->7->9)->(10->11->10)->12)->12)->((0->1->2)->(0->1)->0->2)->(3->4->3)->(0->1->2)->(0->1)->0->2)->((13->14->15)->(13->14)->13->15)->(((0->1->2)->(0->1)->0->2)->(3->4->3)->(0->1->2)->(0->1)->0->2)->(((5->6->5)->((7->8->9)->(7->8)->7->9)->(10->11->10)->12)->12)->((0->1->2)->(0->1)->0->2)->(3->4->3)->(0->1->2)->(0->1)->0->2)->((0->1->2)->(0->1)->0->2)->(3->4->3)->(0->1->2)->(0->1)->0->2
*/


ranSK(N,M,T):-ranSK(random,N,M,T).
  
ranSK(Seed,N,M,T):-
  set_random(seed(Seed)),
  repeat,
    remy_sk(N,X),
    sk_type_of(X,T),
    tsize(T,S),
    S>=M,
  !,
  natvars(T).

sType((A->B->C)->(A->B)->A->C).

kType(A->_B->A).
  
sk_type_of(s,T):-sType(T).
sk_type_of(k,T):-kType(T).
sk_type_of((A*B),T):-
  sk_type_of(A,ST),
  sk_type_of(B,S),
  unify_with_occurs_check(ST,(S->T)). 
  
/*

?- ranSK(60,60,T),ppp(T),ahprove(T),ppp(ok),fail.
(0->(((((((1->2)->1)->1->2)->(1->2)->1)->((1->2)->1)->1->2)->(((1->2)->1)->1->2)->(1->2)->1)->(((((1->2)->1)->1->2)->(1->2)->1)->((1->2)->1)->1->2)->3->2)->4)->0->(((((((1->2)->1)->1->2)->(1->2)->1)->((1->2)->1)->1->2)->(((1->2)->1)->1->2)->(1->2)->1)->(((((1->2)->1)->1->2)->(1->2)->1)->((1->2)->1)->1->2)->3->2)->4
ok

*/

% UNFINISHED - TODO

hilbert_gen(N,Ts):-hilbert_gen_(N,Ts,_).

hilbert_gen_(N,Ts,Vs):-hilbert_gen(N,_,Vs,[],Ts,[]).

hilbert_gen(N1,N3,Vs1,Vs3)-->{ppp(N1)},
  {N1>0,N2 is N1-1},
  ( subst_rule(Vs1,Vs2)
  ; modus_ponens(Vs1,Vs2)
  ),
  ( {N3 is N2-1}
  ; 
    hilbert_gen(N2,N3,Vs2,Vs3)
  ).
  
hilbert_ax( ((A->B->C)->(A->B)->A->C),[A,B,C]).
hilbert_ax((A->B->C),[A,B,C]).
  
subst_rule(NewVs,Vs,[T|Ts],Ts):-
  hilbert_ax(T,Vs),
  member(V,Vs),
  ( V=(X->Y),NewVs=[X,Y|Vs]
  ; NewVs=Vs
  ).

modus_ponens(Vs,Vs,Ts,[T|NewTs]):-
  select(S1,Ts,NewTs),
  select((S2->T),Ts,NewTs),
  unify_with_occurs_check(S1,S2).
  
  

  
