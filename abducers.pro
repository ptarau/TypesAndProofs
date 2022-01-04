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
   
% SIMPLER, NEATER HYPOTHETCAL REASONING CONCEPT
% generalizes ASP, and abductive LP to IPC

% subsets, by increasing size order
subset_of(Xs,Ss):-
  length(Xs,L),
  between(0,L,K),
  ksubset(K,Xs,Ss).

% subsets of K elements of a set with N elements
ksubset(0,_,[]).
ksubset(K,[X|Xs],[X|Rs]):-K>0,K1 is K-1,ksubset(K1,Xs,Rs).
ksubset(K,[_|Xs],Rs):-K>0,ksubset(K,Xs,Rs).

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
    needed_for(ipc_prove,(a->b&c),Hs),
    ppp(Hs)
  )).

abtest2:-
  do((
     needed_for(ipc_prove,((a<->b)<->(~a <-> ~b)),Hs),
     ppp(Hs)
  )).


abtest3:-
  T=(~ (a & b)) <-> (~a v ~b),
  do((
     needed_for(ipc_prove,T, Hs),
     ppp(Hs)
  )).


abtest4:-
  T=((a -> b) & (a-> ~b)),
  do((
     needed_for(ipc_prove,T, Hs),
     ppp(Hs)
  )).


abtest5:-
  T=(a v b -> a & b),
  do((
     needed_for(ipc_prove,T, Hs),
     ppp(Hs)
  )).

abtest6:-
  T=(a v ~a),
  do((
     needed_for(ipc_prove,T, Hs),
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
     needed_for(ipc_prove,T, Hs),
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
     ipc_prove(Hyp&P->G),
     ipc_prove((Hyp&P->IC)),
     not(ipc_prove(Hyp&P->false)),
     ppp(Hyp)
   )).


%% abductive inference for propositions
%% that mast hold for a formula to be a tautology in IPC

needed_for(Formula,R):-
  needed_for(ipc_prove,Formula,Assumption)*-> R=Assumption
; R=fail.


needed_for(Prover,Formula,Assumption):-
  needed_with((&),yes,Prover,Formula,Assumption).

needed_with(_,_,Prover,Formula,Assumption):-
  call(Prover,Formula),
  !,
  Assumption=[].
needed_with(Op,Neg,Prover,Formula,Assumption):-
  % ex: Op=&, Prover = ipc_prove, Formula ((a->b)->c)
  leaves_of(Formula,Ls),
  subset_of(Ls,[P|Positives]),
  mark_hypos(Neg,[P|Positives],Hypos),
  join_with(Op,Hypos,Assumption),
  call(Prover,Assumption->Formula).




% intuitionistic  prover
ipc_prove(T) :- ipc_prove(T,[]).

ipc_prove(true,_):-!.
ipc_prove(A,Vs):-memberchk(A,Vs),!.
ipc_prove(_,Vs):-memberchk(false,Vs),!.
ipc_prove(~A,Vs):-!,ipc_prove(false,[A|Vs]).
ipc_prove(A<->B,Vs):-!,ipc_prove(B,[A|Vs]),ipc_prove(A,[B|Vs]).
ipc_prove((A->B),Vs):-!,ipc_prove(B,[A|Vs]).
ipc_prove((B<-A),Vs):-!,ipc_prove(B,[A|Vs]).
ipc_prove(A & B,Vs):-!,ipc_prove(A,Vs),ipc_prove(B,Vs).
ipc_prove(G,Vs1):- % atomic or disj or false
  select(Red,Vs1,Vs2),
  ipc_prove_reduce(Red,G,Vs2,Vs3),
  !,
  ipc_prove(G,Vs3).
ipc_prove(A v B, Vs):-(ipc_prove(A,Vs);ipc_prove(B,Vs)),!.

ipc_prove_reduce(true,_,Vs1,Vs2):-!,ipc_prove_imp(false,false,Vs1,Vs2).
ipc_prove_reduce(~A,_,Vs1,Vs2):-!,ipc_prove_imp(A,false,Vs1,Vs2).
ipc_prove_reduce((A->B),_,Vs1,Vs2):-!,ipc_prove_imp(A,B,Vs1,Vs2).
ipc_prove_reduce((B<-A),_,Vs1,Vs2):-!,ipc_prove_imp(A,B,Vs1,Vs2).
ipc_prove_reduce((A & B),_,Vs,[A,B|Vs]):-!.
ipc_prove_reduce((A<->B),_,Vs,[(A->B),(B->A)|Vs]):-!.
ipc_prove_reduce((A v B),G,Vs,[B|Vs]):-ipc_prove(G,[A|Vs]).

ipc_prove_imp(true,B,Vs,[B|Vs]):-!.
ipc_prove_imp(~C,B,Vs,[B|Vs]):-!,ipc_prove((C->false),Vs).
ipc_prove_imp((C->D),B,Vs,[B|Vs]):-!,ipc_prove((C->D),[(D->B)|Vs]).
ipc_prove_imp((D<-C),B,Vs,[B|Vs]):-!,ipc_prove((C->D),[(D->B)|Vs]).
ipc_prove_imp((C & D),B,Vs,[(C->(D->B))|Vs]):-!.
ipc_prove_imp((C v D),B,Vs,[(C->B),(D->B)|Vs]):-!.
ipc_prove_imp((C<->D),B,Vs,[((C->D)->((D->C)->B))|Vs]):-!.
ipc_prove_imp(A,B,Vs,[B|Vs]):-memberchk(A,Vs).  

% classical prover - via Glivenko's theorem
cpc_prove(T):-ipc_prove( ~ ~T).

abducibles_of(Formula,Abducibles):-var(Abducibles),!,leaves_of(Formula,Abducibles).
abducibles_of(_,_).

cpc_counterfactual(Abducibles,Formula,Assumption):-
   Prover=cpc_prove,
   WithNeg=yes,
   AggregatorOp=(&),
   counterfactual(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption).

ipc_counterfactual(Abducibles,Formula,Assumption):-
   Prover=ipc_prove,
   WithNeg=yes,
   AggregatorOp=(&),
   counterfactual(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption).

any_counterfactual(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption):-
  abducibles_of(Formula,Abducibles),
  subset_of(Abducibles,Positives),
  mark_hypos(WithNeg,Positives,Hypos),
  join_with(AggregatorOp,Hypos,Assumption),
  call(Prover,Assumption->Formula).


weakest_with(Prover,Gs,G):-
   select(G,Gs,Others),
   \+ (member(O,Others),call(Prover,(G->O))).

counterfactual(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption):-
  findall(Assumption,
    any_counterfactual(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption),
    Assumptions),
  weakest_with(Prover,Assumptions,Assumption).



abduce_explanation_with(Prover,Abductibles,Prog,IC,G,Expl):-
    any_counterfactual(Prover,(&),yes,Abductibles,(Prog->G), Expl),
    call(Prover, Expl & Prog->G),
    call(Prover,(Expl & Prog->IC)),
    \+call(Prover,(Expl & Prog -> false)).


abex(Prover):-
    IC = ~(rained & sunny),
    P = sunny & (rained v sprinkler -> wet),
    As=[sprinkler,rained],
    G = wet,
    do((
     ppp(prog=P),
     ppp(ic=IC),
     abduce_explanation_with(Prover,As,P,IC,G,Explanation),
     ppp('Explanation:' --> Explanation)
   )).


abex2:-abex(ipc_prove).

abex3:-abex(cpc_prove).


intuitionistic_abduction(Abductibles,Prog,IC,G,Expl):-
  Prover=ipc_prove,
  abduce_explanation_with(Prover,Abductibles,Prog,IC,G,Expl).

classical_abduction(Abductibles,Prog,IC,G,Expl):-
  Prover=cpc_prove,
  abduce_explanation_with(Prover,Abductibles,Prog,IC,G,Expl).


mark_hypos(_,[],[]).
mark_hypos(Neg,[P|Ps],[H|Hs]):-
   with_neg(Neg,P,H),
   mark_hypos(Neg,Ps,Hs).

with_neg(_,P,P).
with_neg(yes,P,~P).

join_with_op(_,[],true).
join_with_op(_,[X],X).
join_with_op(Op,[X,Y|Xs],R):-
   join_with_op(Op,[Y|Xs],R0),
   R=..[Op,X,R0].


join_with(Op,Xs,R):-
  memberchk(Op,[(->),(<-)]),!,
  select(Head,Xs,Ys),
  append(Ys,[Head],Zs),
  join_with_op((->),Zs,R).
join_with(Op,Xs,R):-Op=(<->),!,
  permutation(Xs,Ys),
  join_with_op(Op,Ys,R).
join_with(Op,Xs,R):-
  join_with_op(Op,Xs,R).


abtest11:-
  T=(a<-((a<-(b<-d))&(b<-c)&(c<-d))) ,
  ppp(T),
  ipc_prove(T).

abtest12:-
  T=(a<-((a<-(b<-d))&(b<-c))),
  H=(d->c),
  ppp(H->T),
  ipc_prove(T<-H).

cf1(H):-
   T=(a<-((a<-(b<-d))&(b<-c))),
   Prover=ipc_prove,
   WithNeg=yes,
   AggregatorOp=(<-),
   As=[c,d],
   counterfactual(Prover,AggregatorOp,WithNeg,As,T,H).







peirce(Prover,WhatIf):-
    Formula=(((p->q)->p)->p),
    WithNeg=yes,
    AggregatorOp=(v),
    Abducibles=[p],
    counterfactual(Prover,AggregatorOp,WithNeg,Abducibles,Formula,WhatIf).

ipeirce(WhatIf):-
  Prover=ipc_prove,
  peirce(Prover,WhatIf).


cpeirce(WhatIf):-
  Prover=cpc_prove,
  peirce(Prover,WhatIf).
