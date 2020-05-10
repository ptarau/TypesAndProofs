% Generating formulas - see hiking trip, here in Appendix

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

genFormula(N,T):-
  genTree(N,T,Vs),
  natpartitions(Vs).
  
% IIPC prover :  PSPACE-complete -ref to padl 18

prove_ipc(T,ProofTerm):-ljs(ProofTerm,T,[]).

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

% filter if linear

is_linear(X) :- \+ \+ is_linear1(X).

is_linear1(V):-var(V),!,V='$bound'.
is_linear1(l(X,E)):-is_linear1(E),nonvar(X).
is_linear1(a(A,B)):-is_linear1(A),is_linear1(B).

prove_lin(T,ProofTerm):-prove_ipc(T,ProofTerm),is_linear(ProofTerm).

gen_taut(N,T,ProofTerm):-genFormula(N,T),prove_lin(T,ProofTerm).
  
/*

?- show3(3,gen_taut).
   ->
  __|_
 /    \
 0    ->
      _|_
     /   \
    ->    1
     |
    / \
    0  1


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


------ .

    ->
   __|_
  /    \
 ->    ->
  |     |
 / \   / \
 0  0  0  0


  l
  |
 / \
 X  X


------ .

*/

pred(SN,N):-succ(N,SN).

% A024489: 1, 6, 70, 1050, 18018, 336336 ...
lmot(N,E):-succ(N,N1),lmot(E,N,0,N1,0).

lmot(x,A,A,L,L).
lmot(l(E),A1,A2,L1,L3):-pred(L1,L2),lmot(E,A1,A2,L2,L3).
lmot(a(E,F),A1,A4,L1,L3):-pred(A1,A2),
   lmot(E,A2,A3,L1,L2),
   lmot(F,A3,A4,L2,L3).

lclos(N,E):-succ(N,N1),lclos(E,N,0,N1,0,[]).

lclos(X,A,A,L,L,Vs):-member(X,Vs).
lclos(l(X,E),A1,A2,L1,L3,Vs):-pred(L1,L2),lclos(E,A1,A2,L2,L3,[X|Vs]).
lclos(a(E,F),A1,A4,L1,L3,Vs):-pred(A1,A2),
  lclos(E,A2,A3,L1,L2,Vs),
  lclos(F,A3,A4,L2,L3,Vs).

%A062980: 1, 5, 60, 1105, 27120, 828250
llin(N,E):-succ(N,N1),llin(E,N,0,N1,0,[]).

bind_once(V,X):-var(V),V=v(X).
check_binding(V,X):-nonvar(V),V=v(X).

llin(X,A,A,L,L,Vs):-member(V,Vs),bind_once(V,X).
llin(l(X,E),A1,A2,L1,L3,Vs):-pred(L1,L2),
  llin(E,A1,A2,L2,L3,[V|Vs]),
  check_binding(V,X).
  
llin(a(E,F),A1,A4,L1,L3,Vs):-pred(A1,A2),
  llin(E,A2,A3,L1,L2,Vs),
  llin(F,A3,A4,L2,L3,Vs).

% A262301: 1, 3, 26, 367, 7142, 176766, 5,304,356, 186954535
tlin(N,E,T):-succ(N,N1),tlin(E,T,N,0,N1,0,[]).

tlin(l(X,E),(S->T),A1,A2,L1,L3,Vs):-pred(L1,L2),
  tlin(E,T,A1,A2,L2,L3,[V:S|Vs]),
  check_binding(V,X). 
tlin(E,T,A1,A2,L1,L3,Vs):-tlneut(E,T,A1,A2,L1,L3,Vs).

tlneut(X,T,A,A,L,L,Vs):-member(V:TT,Vs),bind_once(V,X),T=TT.
tlneut(a(E,F),T,A1,A4,L1,L3,Vs):-pred(A1,A2),
  tlneut(E,(S->T),A2,A3,L1,L2,Vs),
  tlin(F,S,A3,A4,L2,L3,Vs).

% tlin: much faster that SwissCheese generators
% 238GB if memory -ghc

% tests


  
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

counts_for2(M,Generator,Ks):-
  findall(K,(between(0,M,L),sols_count(call(Generator,L,_),K)),Ks).
  
  
counts_for3(M,Generator,Ks):-
  findall(K,(between(0,M,L),sols_count(call(Generator,L,_,_),K)),Ks).
  
/*
?- time(counts_for(7,tlin,Ks)).
% 8,855,659,045 inferences, 552.730 CPU in 553.015 seconds (100% CPU, 16021680 Lips)
Ks = [1, 3, 26, 367, 7142, 176766, 5304356, 186954535].
*/

:-use_module('third_party/tree_print.pro').

show2(N,Gen):-call(Gen,N,X),ppt(X),nl,fail;true.
show3(N,Gen):-call(Gen,N,X,T),ppt(X),ppt(T),ppp('------'),nl,fail;true.

ppp(X):-portray_clause(X).

% counts

gt(N):-counts_for3(N,gen_taut,Ks),ppp('LinearTautlogies'=Ks).

mc(N):-counts_for2(N,lmot,Ks),ppp('LinearMotzkin'=Ks).

lc(N):-counts_for2(N,llin,Ks),ppp('LinearLambda'=Ks).

lt(N):-counts_for3(N,tlin,Ks),ppp('LinearTermsAndType'=Ks).

  