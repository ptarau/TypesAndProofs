:- op(525,  fy,  ~ ).
:- op(550, xfy,  & ).    % right associative
:- op(575, xfy,  v ).    % right associative
:- op(600, xfx,  <-> ).  % non associative
:- op(800, yfx,  <- ).   % left associative

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

iprover_test:-
   Taut = ((p & q) <-> (((p v q)<->q)<->p)), iprover(Taut),
   Contr=(a & ~a), \+ (iprover(Contr)).

% classical prover - via Glivenko's theorem
cprover(T):-iprover( ~ ~T).

abducibles_of(Formula,Abducibles):-var(Abducibles),!,atoms_of(Formula,Abducibles).
abducibles_of(_,_).

atom_of(A,R):-atomic(A),!,R=A.
atom_of(T,A):-arg(_,T,X),atom_of(X,A).

atoms_of(T,As):-findall(A,distinct(A,atom_of(T,A)),As).

any_protasis(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption):-
  abducibles_of(Formula,Abducibles),
  mark_hypos(WithNeg,Abducibles,Literals),
  subset_of(Literals,Hypos),
  join_with(AggregatorOp,Hypos,Assumption),
  \+ (call(Prover,Assumption->false)),
  call(Prover,Assumption->Formula).

mark_hypos(_,[],[]).
mark_hypos(yes,[P|Ps],[P,~P|Ns]):-mark_hypos(yes,Ps,Ns).
mark_hypos(no,[P|Ps],[P|Ns]):-mark_hypos(no,Ps,Ns).

subset_of(Xs,Ts):-template_from(Xs,Ts),tsubset(Xs,Ts).

template_from(_,[]).
template_from([_|Xs],[_|Zs]):-template_from(Xs,Zs).

tsubset([],[]).
tsubset([X|Xs],[X|Rs]):-tsubset(Xs,Rs).
tsubset([_|Xs],Rs):-tsubset(Xs,Rs).

join_with_op(_,[],true).
join_with_op(_,[X],X).
join_with_op(Op,[X,Y|Xs],R):-join_with_op(Op,[Y|Xs],R0),R=..[Op,X,R0].

join_with(Op,Xs,R):-
  memberchk(Op,[(->),(<-)]),!,
  select(Head,Xs,Ys), append(Ys,[Head],Zs),
  join_with_op((->),Zs,R).

join_with(Op,Xs,R):-Op=(<->),!,permutation(Xs,Ys),join_with_op(Op,Ys,R).

join_with(Op,Xs,R):- join_with_op(Op,Xs,R).

weakest_protasis(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption):-
  findall(Assumption,
    any_protasis(Prover,AggregatorOp,WithNeg,Abducibles,Formula,Assumption),
    Assumptions),
  weakest_with(Prover,Assumptions,Assumption).

weakest_with(_,Gs,G):-memberchk(true,Gs),!,G=true.
weakest_with(Prover,Gs,G):-select(G,Gs,Others),
   \+ (member(Other,Others),weaker_with(Prover,Other,G)).

weaker_with(Prover,P,Q):- \+ call(Prover,P->Q), call(Prover,(Q->P)).

peirce(Prover,WhatIf):-
    Formula=(((p->q)->p)->p),
    WithNeg=yes, AggregatorOp=(v), Abducibles=[p],
    weakest_protasis(Prover,AggregatorOp,WithNeg,Abducibles,Formula,WhatIf).

impl_aggr(H):-
   T=(a<-((a<-(b<-d))&(b<-c))),
   Prover=iprover, WithNeg=yes, AggregatorOp=(->), As=[c,d],
   weakest_protasis(Prover,AggregatorOp,WithNeg,As,T,H).

contra_test(H):-
   T=(p & ~p),
   Prover=iprover, WithNeg=yes, AggregatorOp=(&),
   weakest_protasis(Prover,AggregatorOp,WithNeg,_Abducibles,T,H).

explain_with(Prover,Abducibles,Prog,IC,G,Expl):-
    any_protasis(Prover,(&),yes,Abducibles,(Prog->G), Expl),
    call(Prover, Expl & Prog->G),
    call(Prover,(Expl & Prog->IC)),
    \+ (call(Prover,(Expl & Prog -> false))).

why_wet(Prover):-
    IC = ~(rained & sunny),
    P = sunny & (rained v sprinkler -> wet), As=[sprinkler,rained], G = wet,
    writeln(prog=P), writeln(ic=IC),
    explain_with(Prover,As,P,IC,G,Explanation),
    writeln('Explanation:' --> Explanation).

mints_formula(P)-->[P].                    mints_formula(~P)-->[P].
mints_formula((P->Q))-->[P,Q].             mints_formula((P->Q)->R)-->[P,Q,R].
mints_formula((P->(Q->R)))-->[P,Q,R].      mints_formula((P->(Q v R))) -->[P,Q,R].
mints_formula((P-> ~Q))-->[P,Q].           mints_formula((~P->Q))-->[P,Q].

mints_conjuncts([])-->[].
mints_conjuncts([F|Fs])-->mints_formula(F),mints_conjuncts(Fs).

mints_conjuncts(Atoms,Conjuncts):-mints_conjuncts(Ps,Atoms,[]),sort(Ps,Conjuncts).

any_mints_premise(Prover,Abducibles,Formula,Premise):-
  abducibles_of(Formula,Abducibles),
  subset_of(Abducibles,Chosen),     % select a subset of Abducibles
  template_from(Abducibles,Atoms),  % Atoms is a list of free variables
  part_as_equiv(Atoms,Chosen),      % Chosen provides unique occurrences of Atoms
  mints_conjuncts(Atoms,Conjuncts), % builds the Mints formulas
  join_with_op((&),Conjuncts,Premise), % joins Conjuncts into a conjunction
  \+ (call(Prover,Premise->false)),    % ensures Premise is not a contradiction
  call(Prover,Premise->Formula).       % ensure that Premise implies Formula

part_as_equiv([],[]).
part_as_equiv([U|Xs],[U|Us]):-complement_of(U,Xs,Rs),part_as_equiv(Rs,Us).

complement_of(_,[],[]).
complement_of(U,[X|Xs],NewZs):-complement_of(U,Xs,Zs),place_element(U,X,Zs,NewZs).

place_element(U,U,Zs,Zs).
place_element(_,X,Zs,[X|Zs]).

weakest_mints_premise(Prover,Abducibles,Formula,Premise):-
  findall(Premise,
    any_mints_premise(Prover,Abducibles,Formula,Premise),
    Premises),
  sort(Premises,Uniques),
  weakest_with(Prover,Uniques,Premise).

