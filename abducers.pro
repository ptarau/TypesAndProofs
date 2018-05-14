% simple abductive reasoning - just atomics
% guesses what assumptions (if any) make the formula true


abtest(N,P,ST):-
  allImpFormulas(N,T),
  once(abduce_st(P,T,ST)).

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
  
ord_subterm_of(T,S):-
  findall(U,unique_subterm_of(T,U),Us),
  keysort(Us,Sorted),
  member(_-S,Sorted).

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
 
 /*

 
 there's a strict subformula that turns
 andy failing formula into a taultology?
 
 A->A is one, but that's not interesting
 
 B->(A->B) is another one
 
 open quest:
 
 is the smallest one that does always a subformula?
 
 
 */