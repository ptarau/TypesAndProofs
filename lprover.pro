% IPC prover for implicational fragment, fine-tuned
% to test if proof-terms are affine (i.e., BCK) or linear (i.e., BCI)
% also comes with generator of formulas

% possibly replace -> with -* for better looking linear implication
%:-op(800,xfy,(-*)).

% TYPE go. at the ?- prompt to generate linear taultologies of size 5
% note that variables in formulas are represented as distinct integers
% and as logic variables in the proorf terms

/*
interesting facts:

-all proof terms are closed lambda terms
-most proof terms for small formulas are affine
-very few among the affine are linear
-no linear implicational intuitionistic tautologies of even size

density seems to match results in:

https://arxiv.org/abs/1112.0643

except that they use a different mechanism to count variables

i.e., here we use set partitions - resulting in a total of 
Catalan(N)*Bell(N+1) formula trees

*/

% generators for linear, affine and intuitionistic tautologies
% together with their proof terms

generate_linear(N,T,Proof):-allImpFormulas(N,T),prove_linear(Proof,T).

generate_affine(N,T,Proof):-allImpFormulas(N,T),prove_affine(Proof,T).

generate_intuitionist(N,T,Proof):-allImpFormulas(N,T),prove_ipc(Proof,T).

prove_linear(X,T):-prove_ipc(X,T),is_linear(X).

prove_affine(X,T):-prove_ipc(X,T),is_affine(X).

% prover for implicational fragment of IPC
% using Vorbe'ev/Dychoff/Hudelmeir's claculus

prove_ipc(X,T):-ljs(X,T,[]).

ljs(X,A,Vs):-memberchk(X:A,Vs),!. % leaf variable

ljs(l(X,E),(A->B),Vs):-!,ljs(E,B,[X:A|Vs]).  % lambda term

ljs(E,G,Vs1):- 
  member(_:V,Vs1),head_of(V,G),!, % fail if non-tautology
  select(S:(A->B),Vs1,Vs2),   % source of application
  ljs_imp(T,A,B,Vs2),         % target of application
  !,
  ljs(E,G,[a(S,T):B|Vs2]).    % application
  
ljs_imp(l(X,E),(C->D),B,Vs):-!,ljs(E,(C->D),[X:(D->B)|Vs]).
ljs_imp(E,A,_,Vs):-memberchk(E:A,Vs). 

% omptimization for quicker failure
head_of(_->B,G):-!,head_of(B,G).
head_of(G,G). 

/*
% NOT WORKING !!!
% not all are linear
prove_lpc(X,T):-ljl(X,T,[],[]).

ljl(X,A,Vs1,Vs2):-select(X:A,Vs1,Vs2),!. % leaf variable

ljl(l(X,E),(A->B),Vs1,Vs2):-!,ljl(E,B,[X:A|Vs1],Vs2).  % lambda term

ljl(E,G,Vs1,Vs4):- 
  select(S:(A->B),Vs1,Vs2),   % source of application
  ljl_imp(T,A,B,Vs2,Vs3),         % target of application
  !,
  ljl(E,G,[a(S,T):B|Vs3],Vs4).    % application
  
ljl_imp(l(X,E),(C->D),B,Vs1,Vs2):-!,ljl(E,(C->D),[X:(D->B)|Vs1],Vs2).
ljl_imp(E,A,_,Vs1,Vs2):-select(E:A,Vs1,Vs2),!. 

*/


% extracts leaf variables and their lambda binders
% from a proof term
vars_of(T,Vs,Xs):-vars_of(T,Vs,[],Xs,[]).

vars_of(V,[V|Vs],Vs)-->{var(V)},!.
vars_of(l(K,X),Vs,Us)-->[K],vars_of(X,Vs,Us).
vars_of(a(X,Y),Vs1,Vs3)-->vars_of(X,Vs1,Vs2),vars_of(Y,Vs2,Vs3).

% tests that a lambda term is closed
% that's always true for proof terms
is_closed(X):-
  \+ \+ (
    vars_of(X,Vs,Bs),
    numbervars(Bs,0,_),
    ground(Vs)
  ).

% tests that a lambda term is affine
is_affine(LambdaTerm):-
  vars_of(LambdaTerm,Vars,_Binders),
  sort(Vars,Us),
  length(Vars,LenAll),
  length(Us,LenUniques),
  LenUniques =:= LenAll.

/*
% tests that a lambda term is linear
is_linear(LambdaTerm):-
  vars_of(LambdaTerm,Vars,Binders),
  sort(Vars,Us),
  length(Vars,LenAll),
  length(Us,LenUniques),
  LenUniques =:= LenAll,
  sort(Binders,Sorted),
  Sorted==Us.
*/

% all implicational logic formulas of size N
% A289679		a(n) = Catalan(n-1)*Bell(n).
allImpFormulas(N,T):-
  genTree(N,T,Vs),
  natpartitions(Vs).

% generate trees with N internal nodes and ->/2 for branches   
genTree(N,Tree,Leaves):-genTree(Tree,N,0,Leaves,[]).

genTree(V,N,N)-->[V].
genTree((A->B),SN1,N3)-->{SN1>0,N1 is SN1-1},
  genTree(A,N1,N2),
  genTree(B,N2,N3).

% from set partitions, with 0..N marking distinct variables 
natpartitions(Vs):-natpartitions(Vs,_Ns).

natpartitions(Vs,Ns):-
   mpart_of(Vs,Ns),
   length(Ns,SL),
   succ(L,SL),
   numlist(0,L,Ns).
     
% computes set partitions seen as distinct logic variables
% second arg has the unique variables
mpart_of([],[]).
mpart_of([U|Xs],[U|Us]):-
  mcomplement_of(U,Xs,Rs),
  mpart_of(Rs,Us).

% mimic computing the complement
% but just fuse logic variables
% representing equivalence classes
mcomplement_of(_,[],[]).
mcomplement_of(U,[X|Xs],NewZs):-
  mcomplement_of(U,Xs,Zs),
  mplace_element(U,X,Zs,NewZs).

mplace_element(U,U,Zs,Zs).
mplace_element(_,X,Zs,[X|Zs]).

% linear and affine generators

pred(SX,X):-succ(X,SX).

% counted by https://oeis.org/A281270
affine(N,X,T):-affine(X,[],N,0),type_of(X,T).

affine(X,Vs)-->{member(V,Vs),var(V),V=v(X)}.
affine(l(X,E),Vs)-->pred,affine(E,[V|Vs]),{V=v(X)}.
affine(a(A,B),Vs)-->pred,pred,affine(A,Vs),affine(B,Vs).


% counted by https://oeis.org/A062980
% generated 0,1,0,0,5,0,0,60,0,0,1105,0,0,27120,0,0,828250,0,0,30220800
linear(N,X,T):-linear(X,[],N,0),type_of(X,T).

linear(X,Vs)-->{member(V,Vs),var(V),V=v(X)}.
linear(l(X,E),Vs)-->pred,linear(E,[V|Vs]),{nonvar(V),V=v(X)}.
linear(a(A,B),Vs)-->pred,pred,linear(A,Vs),linear(B,Vs).

is_linear(X) :- \+ \+ is_linear1(X).

is_linear1(V):-var(V),!,V='$bound'.
is_linear1(l(X,E)):-is_linear1(E),nonvar(X).
is_linear1(a(A,B)):-is_linear1(A),is_linear1(B).


% computes type of a linear or affine expression X
type_of(X,T):-type_of(X,T,[]).

type_of(X,T,Vs):-var(X),!,member(X0:T,Vs),X==X0.
type_of(l(X,A),(S->T),Vs):-type_of(A,T,[X:S|Vs]).
type_of(a(A,B),T,Vs):-type_of(A,(S->T),Vs),type_of(B,S,Vs).


% 1,2,3,7,17,36,93,269,723,2085,6583,20271,63867,213994,718043,2431211
% not yet in OEIS
affine_nf(N,X,T):-affine_nf(X,T,[],N,0).

affine_nf(l(X,E),(P->Q),Ps)-->pred,affine_nf(E,Q,[V:P|Ps]),{V=v(X)}.  
affine_nf(X,P,Ps)-->affine_nf_neutral_terms(X,P,Ps).

affine_nf_neutral_terms(X,P,[Y:Q|Ps])--> agrees_and_binds(X:P,[Y:Q|Ps]).
affine_nf_neutral_terms(a(A,B),Q,Ps)-->pred,pred,
  affine_nf_neutral_terms(A,(P->Q),Ps),
  affine_nf(B,P,Ps).

pol(A,X):-pol(1,A,X).

flip(1,0).
flip(0,1).

pol(P,X,R):-var(X),!,R=P:X.
pol(P,A->B,(X->Y)):-flip(P,N),pol(N,A,X),pol(P,B,Y).

agrees_and_binds(X:P,Ps,N,N):-
  member(V:Q,Ps),
  var(V),
  V=v(X),
  P=Q.

% was, with pred.pred for app.  https://oeis.org/A262301
linear_nf(N,X,T):-linear_nf(X,T,[],N,0).

linear_nf(l(X,E),(P->Q),Ps)-->pred,linear_nf(E,Q,[V:P|Ps]),{nonvar(V),V=v(X)}.  
linear_nf(X,P,Ps)-->linear_nf_neutral_terms(X,P,Ps).

linear_nf_neutral_terms(X,P,[Y:Q|Ps])--> agrees_and_binds(X:P,[Y:Q|Ps]).
linear_nf_neutral_terms(a(A,B),Q,Ps)-->pred,
  linear_nf_neutral_terms(A,(P->Q),Ps),
  linear_nf(B,P,Ps).

typed_nf(N,X:T):-typed_nf(X,T,[],N,0).

typed_nf(l(X,E),(P->Q),Ps)-->pred,typed_nf(E,Q,[X:P|Ps]).  
typed_nf(X,P,Ps)-->typed_nf_no_left_lambda(X,P,Ps).

typed_nf_no_left_lambda(X,P,[Y:Q|Ps])--> agrees(X:P,[Y:Q|Ps]).
typed_nf_no_left_lambda(a(A,B),Q,Ps)-->pred,
  typed_nf_no_left_lambda(A,(P->Q),Ps),
  typed_nf(B,P,Ps).

agrees(P,Ps,N,N):-member(Q,Ps),unify_with_occurs_check(P,Q).

tsize(A,R):-var(A),!,R=0.
tsize(A,R):-atomic(A),!,R=0.
tsize((A->B),R):-tsize(A,R1),tsize(B,R2),R is 1+R1+R2.

lsize(A,R):-var(A),!,R=0.
lsize(A,R):-atomic(A),!,R=0.
lsize(l(_,B),R):-lsize(B,R2),R is 1+R2.
lsize(a(A,B),R):-lsize(A,R1),lsize(B,R2),R is 1+R1+R2.

nf_term_type(X,T):-nonvar(T),!,tsize(T,N),nf_term_type(X,T,[],N,0),numvars(X).

nf_term_type(X,T):-nonvar(X),lsize(X,N),nf_term_type(X,T,[],N,0),numvars(T).

nf_term_type(l(X,E),(P->Q),Ps)-->pred,nf_term_type(E,Q,[X:P|Ps]).  
nf_term_type(X,P,Ps)-->nf_neutral(X,P,Ps).

nf_neutral(X,P,Ps)--> {memberchk(X:P,Ps)}.
nf_neutral(a(A,B),Q,Ps)-->pred,
  nf_neutral(A,(P->Q),Ps),
  nf_term_type(B,P,Ps).

numvars(X):-
  term_variables(X,Vs),
  length(Vs,Len),Len1 is Len-1,
  numlist(0,Len1,Vs).

nfaff(N):-nfgo(affine_nf,N).
nflin(N):-nfgo(linear_nf,N).

nfgo(AffOrLin,M):-
  between(0,M,N),ppp(n=N),
  nf_synt(AffOrLin,N).

nf_synt(AffOrLin,N):-
  call(AffOrLin,N,X,T),
  ppt(X),ppt(T),
  numvars(X),numvars(T),
  ppp('------'),nl,
  lsize(X,LS),
  tsize(T,TS),
  assertion(LS=:=TS),
  nf_term_type(XX,T),
  assertion(X=XX),
  nf_term_type(XX,TT),
  assertion(T=TT),
  fail.

% A024489: 1, 6, 70, 1050, 18018, 336336 ...
lmot(N,E,_):-succ(N,N1),lmot(E,N,0,N1,0).

lmot(x,A,A,L,L).
lmot(l(E),A1,A2,L1,L3):-pred(L1,L2),lmot(E,A1,A2,L2,L3).
lmot(a(E,F),A1,A4,L1,L3):-pred(A1,A2),
   lmot(E,A2,A3,L1,L2),
   lmot(F,A3,A4,L2,L3).

lclos(N,E,_):-succ(N,N1),lclos(E,N,0,N1,0,[]).

lclos(X,A,A,L,L,Vs):-member(X,Vs).
lclos(l(X,E),A1,A2,L1,L3,Vs):-pred(L1,L2),lclos(E,A1,A2,L2,L3,[X|Vs]).
lclos(a(E,F),A1,A4,L1,L3,Vs):-pred(A1,A2),
  lclos(E,A2,A3,L1,L2,Vs),
  lclos(F,A3,A4,L2,L3,Vs).

%A062980: 1, 5, 60, 1105, 27120, 828250
llin(N,E,_):-succ(N,N1),llin(E,N,0,N1,0,[]).

llin(X,A,A,L,L,Vs):-member(V,Vs),var(V),V=v(X).
llin(l(X,E),A1,A2,L1,L3,Vs):-pred(L1,L2),
  llin(E,A1,A2,L2,L3,[V|Vs]),
  nonvar(V),
  V=v(X).
llin(a(E,F),A1,A4,L1,L3,Vs):-pred(A1,A2),
  llin(E,A2,A3,L1,L2,Vs),
  llin(F,A3,A4,L2,L3,Vs).

% A262301: 1, 3, 26, 367, 7142, 176766, 5,304,356, 186954535
tlin(N,E,T):-succ(N,N1),tlin(E,T,N,0,N1,0,[]).

tlin(l(X,E),(S->T),A1,A2,L1,L3,Vs):-pred(L1,L2),
  tlin(E,T,A1,A2,L2,L3,[V:S|Vs]),
  nonvar(V),
  V=v(X). 
tlin(E,T,A1,A2,L1,L3,Vs):-tlneut(E,T,A1,A2,L1,L3,Vs).

tlneut(X,T,A,A,L,L,Vs):-member(V:TT,Vs),var(V),V=v(X),T=TT.
tlneut(a(E,F),T,A1,A4,L1,L3,Vs):-pred(A1,A2),
  tlneut(E,(S->T),A2,A3,L1,L2,Vs),
  tlin(F,S,A3,A4,L2,L3,Vs).

/*
?- time(counts_for(7,tlin,Ks)).
% 8,855,659,045 inferences, 552.730 CPU in 553.015 seconds (100% CPU, 16021680 Lips)
Ks = [1, 3, 26, 367, 7142, 176766, 5304356, 186954535].
*/


lgo:-
  A=(((0->1)->0->(1->2)->2)),
  nf_term_type(X,A),
  ppp(here=X),
  lsize(X,XS),ppp(xs=XS),

  nf_term_type(X,B),
  ppp(A=B),
  fail.


:-op(900,xfy,<=).
:-op(900,xfy,=>).

%linfer(U):-ppp(U),nl,fail.

linfer([X:A]<=X<=A):-!.
linfer(G<=T=>A):-var(A),!,
  linfer(G<=T<=A).
linfer(G<=l(X,T)=>(A->B)):-!,
  linfer([X:A|G]<=T=>B).
linfer(GD<=TU<=B):-ppp(TU),ppp(GD),ppp(B),TU=a(T,U),abort,
  append(G,D,GD),
  linfer(G<=T<=(A->B)),
  type_of(T,(A->B),[]),
  linfer(D<=U=>A).


lgo1:-
  A=(((A0->A1)->A0->(A1->A2)->A2)),
  linfer([]<=X=>A),
  ppp(X),
  fail.


% TODO - fold generation and type inference into one - done for ND
% TODO - extend to BCK and BCI algebras
% TODO - use actual B,C,I,K combinators instead of lambda terms
% NOTE: for counting, no need to build term or type !

% generate combinator trees, infer their types - all typable !

/*
% unnecessary - no cycles can form !
type_of(X,T0,Vs):-var(X),!,
   member(X0:T,Vs),X==X0,
   unify_with_occurs_check(T0,T),
   T0=T.
type_of(l(X,A),(S->T),Vs):-type_of(A,T,[X:S|Vs]).
type_of(a(A,B),T,Vs):-type_of(A,(S->T),Vs),type_of(B,S,Vs).
*/



% tools

%ppp(X):-numbervars(X,0,_),writeln(X);fail.

% stats

% counts nb. of solutions of Goal 
sols_count(Goal, Times) :-
        Counter = counter(0),
        (   Goal,
            arg(1, Counter, N0),
            N is N0 + 1,
            nb_setarg(1, Counter, N),
            fail
        ;   arg(1, Counter, Times)
        ).

counts_for(M,Generator,Ks):-
  findall(K,(between(0,M,L),sols_count(call(Generator,L,_,_),K)),Ks).
  
% count linear, affine, and intuitionistic tautologies of up to size 7

lin_counts(Ks):-counts_for(7,generate_linear,Ks).  
aff_counts(Ks):-counts_for(7,generate_affine,Ks).
intuit_counts(Ks):-counts_for(7,generate_intuitionist,Ks).

lin_gen_counts(Ks):-counts_for(7,linear,Ks).  

aff_gen_counts(Ks):-counts_for(7,affine,Ks).

:-include('stats.pro').

go:-generate_linear(5,T,Proof),ppp(formula=T),ppp(proof_term=Proof),nl,fail;true.


ngo:-linear_nf(7,A,B),ppt(A),ppt(B),ppp('---'),nl,fail.

tgo(N):-
  generate_linear(N,T,Proof),
  ppp(formula=T),
  ppt(T),
  namevars(Proof,Proof1),
  ppp(proof_term=Proof1),
  ppt(Proof1),
  nl,
  assertion(is_linear(Proof)),
  fail
; true.


tlgo(N):-
  tlin(N,X,T),ppt(X),ppt(T),ppp('----'),nl,fail.


/*
?- go.
formula=(0->1->(1->0->2)->2)
proof_term=l(A,l(B,l(C,a(a(C,B),A))))

formula=(0->1->(0->1->2)->2)
proof_term=l(A,l(B,l(C,a(a(C,A),B))))

formula=(0->(0->1)->(0->0)->1)
proof_term=l(A,l(B,l(C,a(B,a(C,A)))))

formula=(0->(1->2)->(0->1)->2)
proof_term=l(A,l(B,l(C,a(B,a(C,A)))))

formula=(0->(0->1)->(1->2)->2)
proof_term=l(A,l(B,l(C,a(C,a(B,A)))))

formula=(0->(1->0->2)->1->2)
proof_term=l(A,l(B,l(C,a(a(B,C),A))))

formula=(0->(0->1->2)->1->2)
proof_term=l(A,l(B,l(C,a(a(B,A),C))))

formula=(0->(0->(1->1)->1)->1)
proof_term=l(A,l(B,a(a(B,A),l(C,C))))

formula=((0->1)->0->(0->0)->1)
proof_term=l(A,l(B,l(C,a(A,a(C,B)))))

formula=((0->1)->2->(2->0)->1)
proof_term=l(A,l(B,l(C,a(A,a(C,B)))))

formula=((0->1)->0->(1->2)->2)
proof_term=l(A,l(B,l(C,a(C,a(A,B)))))

formula=((0->1)->(1->2)->0->2)
proof_term=l(A,l(B,l(C,a(B,a(A,C)))))

formula=((0->1)->(2->0)->2->1)
proof_term=l(A,l(B,l(C,a(A,a(B,C)))))

formula=((0->1)->((0->0)->0)->1)
proof_term=l(A,l(B,a(A,a(B,l(C,C)))))

formula=((0->0->0)->0->0->0)
proof_term=l(A,A)

formula=((0->1->0)->0->1->0)
proof_term=l(A,A)

formula=((0->0->1)->0->0->1)
proof_term=l(A,A)

formula=((0->1->2)->1->0->2)
proof_term=l(A,l(B,l(C,a(a(A,C),B))))

formula=((0->1->1)->0->1->1)
proof_term=l(A,A)

formula=((0->1->2)->0->1->2)
proof_term=l(A,A)

formula=((0->(1->1)->1)->0->1)
proof_term=l(A,l(B,a(a(A,B),l(C,C))))

formula=(((0->0)->0)->(0->0)->0)
proof_term=l(A,A)

formula=(((0->1)->0)->(0->1)->0)
proof_term=l(A,A)

formula=(((0->0)->1)->(0->0)->1)
proof_term=l(A,A)

formula=(((0->0)->0)->(0->1)->1)
proof_term=l(A,l(B,a(B,a(A,l(C,C)))))

formula=(((0->1)->1)->(0->1)->1)
proof_term=l(A,A)

formula=(((0->1)->2)->(0->1)->2)
proof_term=l(A,A)

true.


?-linear_nf(4,X,T),ppt(X),ppt(T),writeln('---------'),nl,fail.

   l
  _|_
 /   \
 X    l
     _|
    /  \
    Y   a
        |
       / \
       Y  X


   ->
  __|_
 /    \
 X    ->
      _|_
     /   \
    ->    Y
     |
    / \
    X  Y


---------

   l
  _|_
 /   \
 X    l
     _|
    /  \
    Y   a
        |
       / \
       X  Y


    ->
   __|_
  /    \
 ->    ->
  |     |
 / \   / \
 X  Y  X  Y


---------

   l
  _|_
 /   \
 X    a
     _|
    /  \
    X   l
        |
       / \
       Y  Y


      ->
     __|_
    /    \
   ->     Y
   _|_
  /   \
 ->    Y
  |
 / \
 X  X


---------

false.

?- 
*/
