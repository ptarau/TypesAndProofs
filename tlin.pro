:-op(900,xfy,( '-o' )).

gen_tree(N,Tree,Leaves):-gen_tree(Tree,N,0,Leaves,[]).

gen_tree(V,N,N,[V|Vs],Vs).
gen_tree((A '-o' B),SN1,N3,Vs1,Vs3):-pred(SN1,N1),
  gen_tree(A,N1,N2,Vs1,Vs2),
  gen_tree(B,N2,N3,Vs2,Vs3).
     
pred(SN,N):-succ(N,SN).

mpart_of([],[]).
mpart_of([U|Xs],[U|Us]):-mcomplement_of(U,Xs,Rs),mpart_of(Rs,Us).

mcomplement_of(_,[],[]).
mcomplement_of(U,[X|Xs],NewZs):-
  mcomplement_of(U,Xs,Zs),
  mplace_element(U,X,Zs,NewZs).

mplace_element(U,U,Zs,Zs).
mplace_element(_,X,Zs,[X|Zs]).

partitions(N,Ps):-length(Ps,N),mpart_of(Ps,_).

natpartitions(Vs):-mpart_of(Vs,Ns),
   length(Ns,SL),succ(L,SL),numlist(0,L,Ns).
   
gen_formula(N,T):-gen_tree(N,T,Vs), natpartitions(Vs).

is_linear(X) :- \+ \+ is_linear1(X).

is_linear1(V):-var(V),!,V='$bound'.
is_linear1(l(X,E)):-is_linear1(E),nonvar(X).
is_linear1(a(A,B)):-is_linear1(A),is_linear1(B).

prove_lin(T,ProofTerm):-prove_ipc(T,ProofTerm),is_linear(ProofTerm).

gen_taut(N,T,ProofTerm):-gen_formula(N,T),prove_lin(T,ProofTerm).

linear_motzkin(N,E):-succ(N,N1),linear_motzkin(E,N,0,N1,0).

linear_motzkin(leaf,A,A,L,L).
linear_motzkin(l(E),A1,A2,L1,L3):-pred(L1,L2),linear_motzkin(E,A1,A2,L2,L3).
linear_motzkin(a(E,F),A1,A4,L1,L3):-pred(A1,A2),
  linear_motzkin(E,A2,A3,L1,L2),
  linear_motzkin(F,A3,A4,L2,L3).

closed_almost_linear_term(N,E):-succ(N,N1),
  closed_almost_linear_term(E,N,0,N1,0,[]).

closed_almost_linear_term(X,A,A,L,L,Vs):-member(X,Vs).
closed_almost_linear_term(l(X,E),A1,A2,L1,L3,Vs):-pred(L1,L2),
  closed_almost_linear_term(E,A1,A2,L2,L3,[X|Vs]).
closed_almost_linear_term(a(E,F),A1,A4,L1,L3,Vs):-pred(A1,A2),
  closed_almost_linear_term(E,A2,A3,L1,L2,Vs),
  closed_almost_linear_term(F,A3,A4,L2,L3,Vs).

bind_once(V,X):-var(V),V=v(X).

check_binding(V,X):-nonvar(V),V=v(X).

linear_lambda_term(N,E):-succ(N,N1),linear_lambda_term(E,N,0,N1,0,[]).

linear_lambda_term(X,A,A,L,L,Vs):-member(V,Vs),bind_once(V,X).
linear_lambda_term(l(X,E),A1,A2,L1,L3,Vs):-pred(L1,L2),
  linear_lambda_term(E,A1,A2,L2,L3,[V|Vs]),
  check_binding(V,X). 
linear_lambda_term(a(E,F),A1,A4,L1,L3,Vs):-pred(A1,A2),
  linear_lambda_term(E,A2,A3,L1,L2,Vs),
  linear_lambda_term(F,A3,A4,L2,L3,Vs).

linear_normal_form(N,E):-succ(N,N1),linear_normal_form(E,N,0,N1,0,[]).

linear_normal_form(l(X,E),A1,A2,L1,L3,Vs):-pred(L1,L2),
  linear_normal_form(E,A1,A2,L2,L3,[V|Vs]),check_binding(V,X). 
linear_normal_form(E,A1,A2,L1,L3,Vs):-
  linear_neutral_term(E,A1,A2,L1,L3,Vs).

linear_neutral_term(X,A,A,L,L,Vs):-member(V,Vs),bind_once(V,X).
linear_neutral_term(a(E,F),A1,A4,L1,L3,Vs):-pred(A1,A2),
  linear_neutral_term(E,A2,A3,L1,L2,Vs),
  linear_normal_form(F,A3,A4,L2,L3,Vs).

linear_typed_normal_form(N,E,T):-succ(N,N1),
  linear_typed_normal_form(E,T,N,0,N1,0,[]).

linear_typed_normal_form(l(X,E),(S '-o' T),A1,A2,L1,L3,Vs):-pred(L1,L2),
  linear_typed_normal_form(E,T,A1,A2,L2,L3,[V:S|Vs]),
  check_binding(V,X). 
linear_typed_normal_form(E,T,A1,A2,L1,L3,Vs):-
  linear_neutral_term(E,T,A1,A2,L1,L3,Vs).

linear_neutral_term(X,T,A,A,L,L,Vs):-member(V:TT,Vs),bind_once(V,X),T=TT.
linear_neutral_term(a(E,F),T,A1,A4,L1,L3,Vs):-pred(A1,A2),
  linear_neutral_term(E,(S '-o' T),A2,A3,L1,L2,Vs),
  linear_typed_normal_form(F,S,A3,A4,L2,L3,Vs).

is_polarized_tree(Tree,N):-
  is_polarized_tree(0,Tree,Ps,[],Qs,[]),
  in_bijection(Ps,Qs,N).
  
is_polarized_tree(P,V,Xs1,Xs2,Ys1,Ys2):-atomic(V),!,
  dispatch(P,V,Xs1,Xs2,Ys1,Ys2).
is_polarized_tree(P,(A  '-o' B),Xs1,Xs3,Ys1,Ys3):-Q is 1-P,
  is_polarized_tree(Q,A,Xs1,Xs2,Ys1,Ys2),
  is_polarized_tree(P,B,Xs2,Xs3,Ys2,Ys3).

dispatch(0,X,[X|Xs],Xs,Ys,Ys).
dispatch(1,Y,Xs,Xs,[Y|Ys],Ys).

in_bijection(Ps,Qs, N):-
  length(Ps,K),length(Qs,K),
  sort(Ps,Xs),sort(Qs,Xs),
  length(Xs,K),N is K-1.

prove_polarized(X,T):-is_polarized_tree(T,N),linear_typed_normal_form(N,X,T).

test_reversible_prover(N,X,T):-gen_formula2(N,T),prove_polarized(X,T).

gen_formula2(N,T):-M is 2*N+1, gen_formula(M,T).

prove_ipc(T,ProofTerm):-prove_ipc(ProofTerm,T,[]).

prove_ipc(X,A,Vs):-memberchk(X:A,Vs),!. % leaf variable

prove_ipc(l(X,E),(A '-o' B),Vs):-!,prove_ipc(E,B,[X:A|Vs]).  % lambda term

prove_ipc(E,G,Vs1):- 
  member(_:V,Vs1),head_of(V,G),!, % fast fail if non-tautology
  select(S:(A '-o' B),Vs1,Vs2),   % source of application
  prove_ipc_imp(T,A,B,Vs2),       % target of application
  !,
  prove_ipc(E,G,[a(S,T):B|Vs2]).  % application
  
prove_ipc_imp(l(X,E),(C '-o' D),B,Vs):-!,
  prove_ipc(E,(C '-o' D),[X:(D '-o' B)|Vs]).
prove_ipc_imp(E,A,_,Vs):-memberchk(E:A,Vs). 

% optimization for quicker failure
head_of(_ '-o' B,G):-!,head_of(B,G).
head_of(G,G). 

% linear_typed_normal_form: much faster that SwissCheese generators
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
?- time(counts_for(7,linear_typed_normal_form,Ks)).
% 8,855,659,045 inferences, 552.730 CPU in 553.015 seconds (100% CPU, 16021680 Lips)
Ks = [1, 3, 26, 367, 7142, 176766, 5304356, 186954535].
*/

ppp(X):-portray_clause(X).

ppt(X):-ppp(X).

%:-use_module('third_party/tree_print.pro').

show2(N,Gen):-call(Gen,N,X),ppt(X),nl,fail;true.
show3(N,Gen):-call(Gen,N,X,T),ppt(X),ppt(T),ppp('------'),nl,fail;true.


% counts


gt(N):-counts_for3(N,gen_taut,Ks),ppp('LinearTautologies'(N)=Ks).

mc(N):-counts_for2(N,linear_motzkin,Ks),ppp('LinearMotzkin'(N)=Ks).

lc(N):-counts_for2(N,linear_lambda_term,Ks),ppp('LinearLambda'(N)=Ks).

lt(N):-counts_for3(N,linear_typed_normal_form,Ks),ppp('LinearTermsAndType'(N)=Ks).

tr(N):-counts_for3(N,test_reversible_prover,Ks),ppp('LinearTermsAndType'(N)=Ks).

go:-gt(7),nl,mc(5),nl,lc(5),nl,lt(5),nl,tr(3).

