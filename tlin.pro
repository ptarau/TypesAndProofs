% Generating formulas - see hiking trip, here in Appendix
:-op(900,xfy,( -@ )).

% generate trees with N internal nodes and  -@ /2 for branches   
gen_tree(N,Tree,Leaves):-gen_tree(Tree,N,0,Leaves,[]).

gen_tree(V,N,N)-->[V].
gen_tree((A -@ B),SN1,N3)-->{SN1>0,N1 is SN1-1},
  gen_tree(A,N1,N2),
  gen_tree(B,N2,N3).
     

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

% from set partitions, with 0..N marking distinct variables 

natpartitions(Vs):-
   mpart_of(Vs,Ns),
   length(Ns,SL),
   succ(L,SL),
   numlist(0,L,Ns).
   
gen_formula(N,T):-
  gen_tree(N,T,Vs),
  natpartitions(Vs).
 

% IIPC prover :  PSPACE-complete -ref to padl 18

prove_ipc(T,ProofTerm):-ljs(ProofTerm,T,[]).

ljs(X,A,Vs):-memberchk(X:A,Vs),!. % leaf variable

ljs(l(X,E),(A -@ B),Vs):-!,ljs(E,B,[X:A|Vs]).  % lambda term

ljs(E,G,Vs1):- 
  member(_:V,Vs1),head_of(V,G),!, % fail if non-tautology
  select(S:(A -@ B),Vs1,Vs2),   % source of application
  ljs_imp(T,A,B,Vs2),         % target of application
  !,
  ljs(E,G,[a(S,T):B|Vs2]).    % application
  
ljs_imp(l(X,E),(C -@ D),B,Vs):-!,ljs(E,(C -@ D),[X:(D -@ B)|Vs]).
ljs_imp(E,A,_,Vs):-memberchk(E:A,Vs). 

% omptimization for quicker failure
head_of(_ -@ B,G):-!,head_of(B,G).
head_of(G,G). 

% filter if linear

is_linear(X) :- \+ \+ is_linear1(X).

is_linear1(V):-var(V),!,V='$bound'.
is_linear1(l(X,E)):-is_linear1(E),nonvar(X).
is_linear1(a(A,B)):-is_linear1(A),is_linear1(B).

prove_lin(T,ProofTerm):-prove_ipc(T,ProofTerm),is_linear(ProofTerm).

gen_taut(N,T,ProofTerm):-gen_formula(N,T),prove_lin(T,ProofTerm).
  


/*

?- show3(3,gen_taut).
    -@ 
  __|_
 /    \
 0     -@ 
      _|_
     /   \
     -@     1
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

     -@ 
   __|_
  /    \
  -@      -@ 
  |     |
 / \   / \
 0  0  0  0


  l
  |
 / \
 X  X


------ .

?- time(gt(9)).
'LinearTautlogies'=[0, 1, 0, 4, 0, 27, 0, 315, 0, 5565].
% 29,434,761,596 inferences, 
%  2202.698 CPU in 2203.499 seconds (100% CPU, 13363046 Lips)
true.
*/

% A024489: 1, 6, 70, 1050, 18018, 336336 ...
linear_motzkin(N,E):-succ(N,N1),linear_motzkin(E,N,0,N1,0).

pred(SN,N):-succ(N,SN).

linear_motzkin(x,A,A,L,L).
linear_motzkin(l(E),A1,A2,L1,L3):-pred(L1,L2),
  linear_motzkin(E,A1,A2,L2,L3).
linear_motzkin(a(E,F),A1,A4,L1,L3):-pred(A1,A2),
  linear_motzkin(E,A2,A3,L1,L2),
  linear_motzkin(F,A3,A4,L2,L3).

linear_closed_term(N,E):-succ(N,N1),linear_closed_term(E,N,0,N1,0,[]).

linear_closed_term(X,A,A,L,L,Vs):-member(X,Vs).
linear_closed_term(l(X,E),A1,A2,L1,L3,Vs):-pred(L1,L2),
  linear_closed_term(E,A1,A2,L2,L3,[X|Vs]).
linear_closed_term(a(E,F),A1,A4,L1,L3,Vs):-pred(A1,A2),
  linear_closed_term(E,A2,A3,L1,L2,Vs),
  linear_closed_term(F,A3,A4,L2,L3,Vs).

%A062980: 1, 5, 60, 1105, 27120, 828250
linear_lambda_term(N,E):-succ(N,N1),linear_lambda_term(E,N,0,N1,0,[]).

bind_once(V,X):-var(V),V=v(X).
check_binding(V,X):-nonvar(V),V=v(X).

linear_lambda_term(X,A,A,L,L,Vs):-member(V,Vs),bind_once(V,X).
linear_lambda_term(l(X,E),A1,A2,L1,L3,Vs):-pred(L1,L2),
  linear_lambda_term(E,A1,A2,L2,L3,[V|Vs]),
  check_binding(V,X).
  
linear_lambda_term(a(E,F),A1,A4,L1,L3,Vs):-pred(A1,A2),
  linear_lambda_term(E,A2,A3,L1,L2,Vs),
  linear_lambda_term(F,A3,A4,L2,L3,Vs).

% A262301: 1, 3, 26, 367, 7142, 176766, 5,304,356, 186954535
linear_typed_term(N,E,T):-succ(N,N1),linear_typed_term(E,T,N,0,N1,0,[]).

linear_typed_term(l(X,E),(S -@ T),A1,A2,L1,L3,Vs):-pred(L1,L2),
  linear_typed_term(E,T,A1,A2,L2,L3,[V:S|Vs]),
  check_binding(V,X). 
linear_typed_term(E,T,A1,A2,L1,L3,Vs):-linear_neutral_term(E,T,A1,A2,L1,L3,Vs).

linear_neutral_term(X,T,A,A,L,L,Vs):-member(V:TT,Vs),bind_once(V,X),T=TT.
linear_neutral_term(a(E,F),T,A1,A4,L1,L3,Vs):-pred(A1,A2),
  linear_neutral_term(E,(S -@ T),A2,A3,L1,L2,Vs),
  linear_typed_term(F,S,A3,A4,L2,L3,Vs).


  

% linear_typed_term: much faster that SwissCheese generators
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
  findall(K,
  (between(0,M,L),
    sols_count(call(Generator,L,_),K),S is 2*L+1,ppp(size(L->S):count(K))),
  Ks).
  
  
counts_for3(M,Generator,Ks):-
  findall(K,
  (between(0,M,L),
    sols_count(call(Generator,L,_,_),K),S is 2*L+1,ppp(size(L->S):count(K))),
  Ks).
  
/*
% A262301
?- time(counts_for(7,linear_typed_term,Ks)).
% 8,855,659,045 inferences, 552.730 CPU in 553.015 seconds (100% CPU, 16021680 Lips)
Ks = [1, 3, 26, 367, 7142, 176766, 5304356, 186954535].
*/

:-use_module('third_party/tree_print.pro').

show2(N,Gen):-call(Gen,N,X),ppt(X),nl,fail;true.
show3(N,Gen):-call(Gen,N,X,T),ppt(X),ppt(T),ppp('------'),nl,fail;true.

ppp(X):-portray_clause(X).

% counts


gt(N):-counts_for3(N,gen_taut,Ks),ppp('LinearTautologies'(N)=Ks).

mc(N):-counts_for2(N,linear_motzkin,Ks),ppp('LinearMotzkin'(N)=Ks).

lc(N):-counts_for2(N,linear_lambda_term,Ks),ppp('LinearLambda'(N)=Ks).

lt(N):-counts_for3(N,linear_typed_term,Ks),ppp('LinearTermsAndType'(N)=Ks).

go:-gt(7),nl,mc(5),nl,lc(5),nl,lt(5).