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
  
abduce_impl(N):-
  Generator=genTree,Prover=eprove,
  abduce_atomics(N,Generator,Prover).

abduce_full(N):-
  Generator=genOpTree,Prover=faprove,
  abduce_atomics(N,Generator,Prover).
  
abduce_atomics(N,Generator,Prover):-
  call(Generator,N, T, Vs),
  natpartitions(Vs,Us),% length(Us,Len),  
  (
    abduce_with_prover(Prover,G,T,Us,Assumptions,NewT)->
    ( Assumptions=[]->ppp(tautology:T)
    ; ppp(Assumptions:(NewT->G))
    )
  ; ppp(unabducible:T)
  ),
  fail.

abduce_with_prover(Prover,G,T,Us,Assumptions,NewT):-
  length(Us,N),N1 is N-1,
  select(G,Us,OtherUs),
  between(0,N1,K),
  ksubset(K,OtherUs,Assumptions),
  list2impl(Assumptions,T->G,NewT),
  %ppp(Us:T->G),
  call(Prover,NewT).

list2impl([],G,G).
list2impl([X|Xs],G,(X->R)):-list2impl(Xs,G,R).


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


% subsets of K elements of a set with N elements
ksubset(0,_,[]).
ksubset(K,[X|Xs],[X|Rs]):-K>0,K1 is K-1,ksubset(K1,Xs,Rs).
ksubset(K,[_|Xs],Rs):-K>0,ksubset(K,Xs,Rs).

% subsets, by increasing size order
ksubset_of(Xs,Ss):-
  length(Xs,L),
  between(0,L,K),
  ksubset(K,Xs,Ss).


leaves_of(Form,Leaves):-
  leaves_of(Form,Xs,[]),
  sort(Xs,Leaves).

leaves_of([])-->!,[].
leaves_of(false)-->!,[].
leaves_of(true)-->!,[].
leaves_of(A)-->{atomic(A)},!,[A].
leaves_of(T)-->
  {T=..[_|Xs]},
  leaves_of_all(Xs).

leaves_of_all([])-->[].
leaves_of_all([X|Xs])-->leaves_of(X),leaves_of_all(Xs).



abtest1:-
  do((
    needed_for(iprover,(a->b&c),Hs),
    ppp(Hs)
  )).

abtest2:-
  do((
     needed_for(iprover,((a<->b)<->(~a <-> ~b)),Hs),
     ppp(Hs)
  )).


abtest3:-
  T=(~ (a & b)) <-> (~a v ~b),
  do((
     needed_for(iprover,T, Hs),
     ppp(Hs)
  )).


abtest4:-
  T=((a -> b) & (a-> ~b)),
  do((
     needed_for(iprover,T, Hs),
     ppp(Hs)
  )).


abtest5:-
  T=(a v b -> a & b),
  do((
     needed_for(iprover,T, Hs),
     ppp(Hs)
  )).

abtest6:-
  T=(a v ~a),
  do((
     needed_for(iprover,T, Hs),
     ppp(Hs)
  )).

abtest7:-
   T= (
     (a -> b<->c)  &
     (b<->bb) &
     (c<->cc) &
     (a -> aa) ->
     bb<->cc
     ),
     do((
     needed_for(iprover,T, Hs),
     ppp(Hs)
     )).

abtest8:-
  T=(p -> q v r),
  do((
     needed_for(T, Hs),
     ppp(Hs)
  )).

% peirce's law
abtest9:-
  T=(((p->q)->p)->p),
   do((
     needed_for(T, Hs),
     ppp(Hs)
  )).

abtest10:-
  T=((a&b->c)->((a->c) v (b->c))),
  do((
     needed_for(T, Hs),
     ppp(Hs)
  )).




asp1:-
   T= ((p <- ~q) & (q <- ~p)),
   do((
     needed_for(T, Hs),
     ppp(Hs)
  )).

asp2:-
   T= (
     p &
     (r<-p&q) &
     (s<-p& ~q)
   ),
   do((
     needed_for(~ ~T, Hs),
     ppp(Hs)
  )).


asp3:-
   T= (
     p &
     (r<-p&q) &
     (s<-p& ~q)
   ),
   do((
     needed_for(T, Hs),
     ppp(Hs)
   )).


abex1:-
    IC = ~(rained & sunny),
    P = sunny & (rained v sprinkler -> wet),
    G = wet,
    do((
     ppp(prog=P),
     ppp(ic=IC),
     needed_for(P->G, Hyp),
     iprover(Hyp&P->G),
     iprover((Hyp&P->IC)),
     not(iprover(Hyp&P->false)),
     ppp(Hyp)
   )).


%% abductive inference for propositions
%% that mast hold for a formula to be a tautology in IPC

needed_for(Formula,R):-
  needed_for(iprover,Formula,Assumption)*-> R=Assumption
; R=fail.


needed_for(Prover,Formula,Assumption):-
  needed_with((&),yes,Prover,Formula,Assumption).

needed_with(_,_,Prover,Formula,Assumption):-
  call(Prover,Formula),
  !,
  Assumption=[].
needed_with(Op,Neg,Prover,Formula,Assumption):-
  % ex: Op=&, Prover = iprover, Formula ((a->b)->c)
  atoms_of(Formula,Ls),
  subset_of(Ls,[P|Positives]),
  mark_hypos(Neg,[P|Positives],Hypos),
  join_with(Op,Hypos,Assumption),
  call(Prover,Assumption->Formula).

  
