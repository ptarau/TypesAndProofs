% SIMPLER, NEATER HYPOTHETCAL REASONING CONCEPT
% generalizes ASP, and abductive LP to IPC



% intuitionistic  iprover
iprover(T) :- iprover(T,[]).

iprover(true,_):-!.
iprover(A,Vs):-memberchk(A,Vs),!.
iprover(_,Vs):-memberchk(false,Vs),!.
iprover(~A,Vs):-!,iprover(false,[A|Vs]).
iprover(A<->B,Vs):-!,iprover(B,[A|Vs]),iprover(A,[B|Vs]).
iprover((A->B),Vs):-!,iprover(B,[A|Vs]).
iprover((B<-A),Vs):-!,iprover(B,[A|Vs]).
iprover(A & B,Vs):-!,iprover(A,Vs),iprover(B,Vs).
iprover(G,Vs1):- % atomic or disj or false
  select(Red,Vs1,Vs2),
  iprover_reduce(Red,G,Vs2,Vs3),
  !,
  iprover(G,Vs3).
iprover(A v B, Vs):-(iprover(A,Vs) ; iprover(B,Vs)),!.

iprover_reduce(true,_,Vs1,Vs2):-!,iprover_impl(false,false,Vs1,Vs2).
iprover_reduce(~A,_,Vs1,Vs2):-!,iprover_impl(A,false,Vs1,Vs2).
iprover_reduce((A->B),_,Vs1,Vs2):-!,iprover_impl(A,B,Vs1,Vs2).
iprover_reduce((B<-A),_,Vs1,Vs2):-!,iprover_impl(A,B,Vs1,Vs2).
iprover_reduce((A & B),_,Vs,[A,B|Vs]):-!.
iprover_reduce((A<->B),_,Vs,[(A->B),(B->A)|Vs]):-!.
iprover_reduce((A v B),G,Vs,[B|Vs]):-iprover(G,[A|Vs]).

iprover_impl(true,B,Vs,[B|Vs]):-!.
iprover_impl(~C,B,Vs,[B|Vs]):-!,iprover((C->false),Vs).
iprover_impl((C->D),B,Vs,[B|Vs]):-!,iprover((C->D),[(D->B)|Vs]).
iprover_impl((D<-C),B,Vs,[B|Vs]):-!,iprover((C->D),[(D->B)|Vs]).
iprover_impl((C & D),B,Vs,[(C->(D->B))|Vs]):-!.
iprover_impl((C v D),B,Vs,[(C->B),(D->B)|Vs]):-!.
iprover_impl((C<->D),B,Vs,[((C->D)->((D->C)->B))|Vs]):-!.
iprover_impl(A,B,Vs,[B|Vs]):-memberchk(A,Vs).  

% classical prover - via Glivenko's theorem
cprover(T):-iprover( ~ ~T).
cprover(T,Vs):-iprover( ~ ~T,Vs).


abducibles_of(Formula,Abducibles):-var(Abducibles),!,atoms_of(Formula,Abducibles).
abducibles_of(_,_).

subset_of(Xs,Ts):-
  template_from(Xs,Ts),
  tsubset(Xs,Ts).

template_from(_,[]).
template_from([_|Xs],[_|Zs]):-template_from(Xs,Zs).

tsubset([],[]).
tsubset([X|Xs],[X|Rs]):-tsubset(Xs,Rs).
tsubset([_|Xs],Rs):-tsubset(Xs,Rs).

atom_of(A,R):-atomic(A),!,R=A.
atom_of(T,A):-arg(_,T,X),atom_of(X,A).

atoms_of(T,As):-
  findall(A,distinct(A,atom_of(T,A)),As).

% protasis generation

any_protasis(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption):-
  abducibles_of(Formula,Abducibles),
  mark_hypos(WithNeg,Abducibles,Literals),
  subset_of(Literals,Hypos),
  join_with(AggregatorOp,Hypos,Assumption),
  \+ (call(Prover,Assumption->false)),
  call(Prover,Assumption->Formula).

mark_hypos(_,[],[]).
mark_hypos(yes,[P|Ps],[P,~P|Ns]):-
  mark_hypos(yes,Ps,Ns).
mark_hypos(no,[P|Ps],[P|Ns]):-
  mark_hypos(no,Ps,Ns).

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

weakest_protasis(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption):-
  findall(Assumption,
    any_protasis(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption),
    Assumptions),
  weakest_with(Prover,Assumptions,Assumption).


%weaker_with(Prover,P,Q):-call(Prover,~(P->Q) v (Q->P)).
weaker_with(Prover,P,Q):- \+call(Prover,P->Q), call(Prover,(Q->P)).

weakest_with(_,Gs,G):-memberchk(true,Gs),!,G=true.
weakest_with(Prover,Gs,G):-
   select(G,Gs,Others),
   \+ (member(Other,Others),weaker_with(Prover,Other,G)).



/*
trim_equivs(_,[],[]).
trim_equivs(P,[F|Fs],[F|Rs]):-
  \+ (member(G,Fs),call(P,F<->G)),!,
  trim_equivs(P,Fs,Rs).
trim_equivs(P,[_|Fs],Rs):-
  trim_equivs(P,Fs,Rs).


weakest_with(_,[],[]).
weakest_with(Prover,[G|Gs],[G|Ws]):-
  \+ (member(Other,Gs),call(Prover,(G->Other))),!,
  weakest_with(Prover,Gs,Ws).
weakest_with(Prover,[_|Gs],Ws):-weakest_with(Prover,Gs,Ws).
*/


explain_with(Prover,Abducibles,Prog,IC,G,Expl):-
    any_protasis(Prover,(&),yes,Abducibles,(Prog->G), Expl),
    call(Prover, Expl & Prog->G),
    call(Prover,(Expl & Prog->IC)),
    \+ call(Prover,(Expl & Prog -> false)).


%%%% examples

iprover_test:-
   Taut = ( (p & q) <-> (((p v q)<->q)<->p) ),
   iprover(Taut),
   Contr=(a & ~a),
   not(iprover(Contr)).



% examples



peirce(Prover,WhatIf):-
    Formula=(((p->q)->p)->p),
    WithNeg=yes,
    AggregatorOp=(v),
    Abducibles=[p],
    weakest_protasis(Prover,AggregatorOp,WithNeg,Abducibles,Formula,WhatIf).

contra_test(H):-
   T=(p & ~p),
   Prover=iprover,
   WithNeg=yes,
   AggregatorOp=(&),
   weakest_protasis(Prover,AggregatorOp,WithNeg,_Abducibles,T,H).


impl_aggr(H):-
   T=(a<-((a<-(b<-d))&(b<-c))),
   Prover=iprover,
   WithNeg=yes,
   AggregatorOp=(<-),
   As=[c,d],
   weakest_protasis(Prover,AggregatorOp,WithNeg,As,T,H).

ipeirce(WhatIf):-
  Prover=iprover,
  peirce(Prover,WhatIf).


cpeirce(WhatIf):-
  Prover=cprover,
  peirce(Prover,WhatIf).

% abduction test

why_wet:-why_wet(iprover).

why_wet(Prover):-
    IC = ~(rained & sunny),
    P = sunny & (rained v sprinkler -> wet),
    As=[sprinkler,rained],
    G = wet,
    writeln(prog=P),
    writeln(ic=IC),
    explain_with(Prover,As,P,IC,G,Explanation),
    writeln('Explanation:' --> Explanation).


% other tests

impl_test1:-
  T=(a<-((a<-(b<-d))&(b<-c)&(c<-d))) ,
  writeln(T),
  iprover(T).

impl_test2:-
  T=(a<-((a<-(b<-d))&(b<-c))),
  H=(d->c),
  writeln(H->T),
  iprover(T<-H).




model_test1(A):-
  P=((a <- ~b) & (b <- ~a) & (c <- a) & (c <- b)->c),
  weakest_protasis(iprover,(&),yes,[a,b],P,A).

model_test2(A):-
  P=((a <- ~b) & (b <- ~a) & (c <- a) & (c <- b)->c),
  weakest_protasis(cprover,(&),yes,[a,b],P,A).


model_test3(A):-
  P=((a<->b)<->(~a <-> ~b)),
  weakest_protasis(iprover,(&),yes,[a,b],P,A).

model_test4(A):-
  P=((a<->b)<->(~a <-> ~b)),
  weakest_protasis(cprover,(&),yes,[a,b],P,A).


ht_axiom(A):-
  P=(f v (f->g) v ~g),
  weakest_protasis(iprover,(->),yes,_,P,A).

ht_theorem1(A):-
  P =(f v (f->g) v ~g),
  Q = (~f v ~ ~ f),
  weakest_protasis(iprover,(v),yes,_,P->Q,A).

ht_theorem2(A):-
  P = (~f v ~ ~ f),
  Q = ((~f v g) <-> (~ ~ f -> g)),
  weakest_protasis(iprover,(v),yes,_,P->Q,A).

ht_theorem3(A):-
   P = (f v (f->g) v ~g),
   Q = ~ (f v g) <-> ~f & ~ g,
   R = ~ (f & g) <-> ~f v ~ g,
   weakest_protasis(iprover,(v),yes,_,P->Q&R,A).


%%%%%%%%%%%%%%%% specializations


intuitionistic_protasis(Abducibles,Formula,Assumption):-
   Prover=iprover,
   WithNeg=yes,
   AggregatorOp=(&),
   weakest_protasis(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption).

classical_protasis(Abducibles,Formula,Assumption):-
   Prover=cprover,
   WithNeg=yes,
   AggregatorOp=(&),
   weakest_protasis(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption).


intuitionistic_abduction(Abducibles,Prog,IC,G,Expl):-
  Prover=iprover,
  explain_with(Prover,Abducibles,Prog,IC,G,Expl).

classical_abduction(Abducibles,Prog,IC,G,Expl):-
  Prover=cprover,
  explain_with(Prover,Abducibles,Prog,IC,G,Expl).


genAssumption(N,Ops,Tree,Leaves):-
  genAssumption(Ops,Tree,N,0,Leaves,[]).
    
genAssumption(_,V,N,N)-->[V].
genAssumption(Ops,OpAB,SN1,N3)-->
  { SN1>0,N1 is SN1-1,
    member(Op,Ops),apply_oper2(Op,A,B,OpAB)
  },
  genAssumption(Ops,A,N1,N2),
  genAssumption(Ops,B,N2,N3).
  
apply_oper2(Op,A,B,OpAB):-functor(OpAB,Op,2),arg(1,OpAB,A),arg(2,OpAB,B).


mints_formula(P)-->[P].
mints_formula(~P)-->[P].
mints_formula((P->Q))-->[P,Q].
mints_formula((P->Q)->R)-->[P,Q,R].
mints_formula((P->(Q->R)))-->[P,Q,R].
mints_formula((P->(Q v R))) -->[P,Q,R].
mints_formula((P-> ~Q))-->[P,Q].
mints_formula((~P->Q))-->[P,Q].
%mints_formula((P v Q)) -->[P,Q].

mints_conjuncts([])-->[].
mints_conjuncts([F|Fs])-->
  mints_formula(F),
  mints_conjuncts(Fs).


mints_conjuncts(Atoms,Conjuncts):-mints_conjuncts(Ps,Atoms,[]),sort(Ps,Conjuncts).

any_mints_premise(Prover,Abducibles,Formula,Premise):-
  abducibles_of(Formula,Abducibles),
  subset_of(Abducibles,Chosen),
  template_from(Abducibles,Atoms),
  mpart_of(Atoms,Chosen),
  mints_conjuncts(Atoms,Conjuncts),
  join_with_op((&),Conjuncts,Premise),
  \+ (call(Prover,Premise->false)),
  call(Prover,Premise->Formula).

weakest_mints_premise(Prover,Abducibles,Formula,Premise):-
  findall(Premise,
    any_mints_premise(Prover,Abducibles,Formula,Premise),
    Premises),
  sort(Premises,Uniques),
  weakest_with(Prover,Uniques,Premise).


imtest(Formula,Premise):-
  weakest_mints_premise(iprover,_Abducibles,Formula,Premise).

cmtest(Formula,Premise):-
  weakest_mints_premise(cprover,_Abducibles,Formula,Premise).

acmtest(Formula,Premise):-
  any_mints_premise(cprover,_Abducibles,Formula,Premise).

imt1:-
  T=((p->(q v r))),
  R=((p->h) & (h<->(q v r))),
  iprover(T<-R).

imt2(P):-
  T=((p->(q v r))),
  R=((p->h) & (h<->(q v r))),
  intuitionistic_protasis(_,T->R,P).

path_test:-
  Cs=[a<-b v c,b<-d v e v f,e<-a,f<-b v e,c<-e v f,e<-g v h],
  %Cs=[a<-b,b<-c v d,c<-e v h,e<-a],
  G=(h->a),
  iprover(G,Cs).



path_inf:-
  Graph=(a<-b v c)&(c<-d v e)&(e<-a),
  Link=((a->d)),
  Formula=(Graph->Link),
  weakest_protasis(iprover,(->),no,_Abducibles,Formula,Assumption),
  ppp(Assumption),
  fail.

path_inf1:-
  Graph=((a->b)&(b->c)&(c->a) & (d->e)&(e->f)&(f->d)),
  Link=((f->a)),
  Formula=(Graph->Link),
  %weakest_mints_premise(iprover,_Abducibles,Formula,Assumption),
  weakest_protasis(iprover,(->),no,_Abducibles,Formula,Assumption),
  ppp(Assumption),
  fail.


acall(Abducible, Holes):-
   copy_term(Abducible,Caller),
   term_variables(Abducible,Us),
   copy_term(Abducible+Us,Caller+Vs),
   call(Caller),
   find_holes(Us,Vs,Holes).

find_holes([],[],[]).
find_holes([U|Us],[V|Vs],URs):-nonvar(V),!,URs=[U-V|Rs],find_holes(Us,Vs,Rs).
find_holes([_|Us],[_|Vs],Rs):-find_holes(Us,Vs,Rs).


acall_test:-
  G=append(_,_,[1,2,3]),
  acall(G,R),
  ppp(G-->R),
  fail.

/*

p -> o1 & o2 & o3.

p <- i1 v i2 v i3 v i4

?- A=((the->cat)&(big->cat)),B=(the v big -> cat),iprover(A<->B).
A =  (the->cat)&(big->cat),
B =  (the v big->cat).


*/
