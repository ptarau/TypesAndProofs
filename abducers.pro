% simple abductive reasoning - just atomics
% guess what assumptions (if any) make the formula true
% guess what assumptions make a true formula false if removed

%trimleaf(A,A):-atomic(A).
trimleaf(A->B,B):-atomic(A).
trimleaf(A->B,A):-atomic(B).
trimleaf(A->B,X->B):-trimleaf(A,X).
trimleaf(A->B,A->Y):-trimleaf(B,Y).

abtest(N,P,ST):-
  allImpFormulas(N,T),
  once(abduce_st(P,T,ST)).

ab(FT,TT):-abduce_st(pprove,FT,TT).

abduce_st(P,T,T):-call(P,T).
abduce_st(P,T,(S->T)):-
  ord_subterm_of(T,S),T\=S,
  call(P,(S->T)).
  

unique_subterm_of(T,N-S):- 
  distinct(S,subterm_of(T,S)),
  tsize(S,N).
  
subterm_of(Term,X) :-
  compound(Term),
  arg(_,Term,Arg),
  subterm_of(Arg,X).
subterm_of(X, X).
  

rev_ord_subterm_of(T,S):-
  findall(U,unique_subterm_of(T,U),Us),
  revkeysort(Us,Sorted),
  member(_-S,Sorted).
 
revkeysort(RXs, RevRXs) :-
  sort(1, @>=, RXs, RevRXs).

ord_subterm_of(T,S):-
  findall(U,unique_subterm_of(T,U),Us),
  keysort(Us,Sorted),
  member(_-S,Sorted).

  
subterms_of(T,Ts):-
  findall(U,(unique_subterm_of(T,U),\+arg(2,U,T)),Us),
  keysort(Us,Sorted),
  maplist(arg(2),Sorted,Ts).
  
abduce_imp(P,T,T):-call(P,T).
abduce_imp(P,T,Fixed):-
  maxvar(T,M), 
  between(0,M,K),
  allImpFormulas(K,Imp),
  Fixed=(Imp->T),
  call(P,Fixed).
  

abduce_for1(P,T0,Hyps):-
  maxvar(T0,M),numlist(0,M,Ns),
  toHorn(T0,T),
  between(0,M,K),
  ksubset(K,Ns,Hyps),
  call(P,(T:-Hyps)).
  
  
abduce_for2(P,T0,Hyps):-
  maxvar(T0,M),numlist(0,M,Ns),
  toHorn(T0,T),
  genWithArrows(Ns,Arrs),
  between(0,M,K),
  ksubset(K,Arrs,Hyps),
  call(P,(T:-Hyps)).
  

abtest(N,P):-
  N1 is N-1,
  genTree(N, T, Vs),
  natpartitions(Vs,Us),  
  %ppp(Us),
  select(G,Us,OtherUs),
  %\+call(P,(T->G)),
  once((
    between(0,N1,K),
    ksubset(K,OtherUs,SomeUs),
    list2impl(SomeUs,T->G,Abduced),
    %ppp(Us:T->G),
    call(P,Abduced)
  )),
  %ppp(Us->T->G),
  ppp(SomeUs:(T->G)),
  fail.
  
list2impl([],G,G).
list2impl([X|Xs],G,(X->R)):-list2impl(Xs,G,R).
  
  % subsets of K elements of a set with N elements
ksubset(0,_,[]).
ksubset(K,[X|Xs],[X|Rs]):-K>0,K1 is K-1,ksubset(K1,Xs,Rs).
ksubset(K,[_|Xs],Rs):-K>0,ksubset(K,Xs,Rs).

genWithArrows(Ns,Hyps):-
  findall(Arr,genArrow(Ns,Arr),As),
  append(Ns,As,Hyps).
  
genArrow(Ns,Arr):-
 member(A,Ns),
 member(B,Ns),
 A\=B,
 member(Arr,[(A->B),(B->A)]).
 
 
 
 % assume Horn, for simplicity - minimal abduction trivial
 
trivYes((H:-Bs),(H:-[H|Bs])).
 
trivNo((H:-Bs),(H:-Ds)):-
   delete(H,Bs,Cs),
   delete((H:-_),Cs,Ds).
   
% what atomics being true make a formula true 

abduceFull(P,T0,T,Hyps):-abduceFullCand(T0,T,Hyps),call(P,T),!.

abduceFullCand(T0,T,Hyps)  :-
  maxvar(T0,M),
  numlist(0,M,Ns),
  between(0,M,K),
  ksubset(K,Ns,Hyps),
  toHorn(T,(T0:-Hyps)).

 
abduce_test(N,P):-
   allSortedFullFormulas(N,T0),
   \+call(P,T0),
   abduceFull(P,T0,T,Hyps),Hyps=[_|_],
   ppp((T0-->T)),
   fail.
   
  
  
 