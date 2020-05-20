% Generating formulas - see hiking trip, here in Appendix

:-op(900,xfy,( '-o' )).

% generate trees with N internal nodes and  '-o' /2 for branches     
gen_tree(N,Tree,Leaves):-gen_tree(Tree,N,0,Leaves,[]).

gen_tree(V,N,N,[V|Vs],Vs).
gen_tree((A '-o' B),SN1,N3,Vs1,Vs3):-pred(SN1,N1),
  gen_tree(A,N1,N2,Vs1,Vs2),
  gen_tree(B,N2,N3,Vs2,Vs3).
     
pred(SN,N):-succ(N,SN).

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
 
gen_formula2(N,T):-
  M is 2*N+1,
  gen_formula(M,T).
  
  
prove_ipc(T,ProofTerm):-prove_ipc(ProofTerm,T,[]).

prove_ipc(X,A,Vs):-memberchk(X:A,Vs),!. % leaf variable

prove_ipc(l(X,E),(A '-o' B),Vs):-!,prove_ipc(E,B,[X:A|Vs]).  % lambda term

prove_ipc(E,G,Vs1):- 
  member(_:V,Vs1),head_of(V,G),!, % fail if non-tautology
  select(S:(A '-o' B),Vs1,Vs2),   % source of application
  prove_ipc_imp(T,A,B,Vs2),         % target of application
  !,
  prove_ipc(E,G,[a(S,T):B|Vs2]).    % application
  
prove_ipc_imp(l(X,E),(C '-o' D),B,Vs):-!,
  prove_ipc(E,(C '-o' D),[X:(D '-o' B)|Vs]).
prove_ipc_imp(E,A,_,Vs):-memberchk(E:A,Vs). 

% omptimization for quicker failure
head_of(_ '-o' B,G):-!,head_of(B,G).
head_of(G,G). 

% filter if linear

is_linear(X) :- \+ \+ is_linear1(X).

is_linear1(V):-var(V),!,V='$bound'.
is_linear1(l(X,E)):-is_linear1(E),nonvar(X).
is_linear1(a(A,B)):-is_linear1(A),is_linear1(B).

prove_lin(T,ProofTerm):-prove_ipc(T,ProofTerm),is_linear(ProofTerm).

gen_taut(N,T,ProofTerm):-gen_formula(N,T),prove_lin(T,ProofTerm).
  


/*
?- time(gt(9)).
'LinearTautlogies'=[0, 1, 0, 4, 0, 27, 0, 315, 0, 5565].
% 29,434,761,596 inferences, 
%  2202.698 CPU in 2203.499 seconds (100% CPU, 13363046 Lips)
true.
*/

% A024489: 1, 6, 70, 1050, 18018, 336336 ...
linear_motzkin(N,E):-succ(N,N1),linear_motzkin(E,N,0,N1,0).


linear_motzkin(x,A,A,L,L).
linear_motzkin(l(E),A1,A2,L1,L3):-pred(L1,L2),
  linear_motzkin(E,A1,A2,L2,L3).
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

  
% A262301: 1, 3, 26, 367, 7142, 176766
  
linear_normal_form(N,E):-succ(N,N1),
  linear_normal_form(E,N,0,N1,0,[]).

linear_normal_form(l(X,E),A1,A2,L1,L3,Vs):-pred(L1,L2),
  linear_normal_form(E,A1,A2,L2,L3,[V|Vs]),
  check_binding(V,X). 
linear_normal_form(E,A1,A2,L1,L3,Vs):-
  linear_neutral_term(E,A1,A2,L1,L3,Vs).

linear_neutral_term(X,A,A,L,L,Vs):-member(V,Vs),bind_once(V,X).
linear_neutral_term(a(E,F),A1,A4,L1,L3,Vs):-pred(A1,A2),
  linear_neutral_term(E,A2,A3,L1,L2,Vs),
  linear_normal_form(F,A3,A4,L2,L3,Vs).
  
  
% A262301: 1, 3, 26, 367, 7142, 176766, 5,304,356, 186954535
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
    sols_count(call(Generator,L,_),K),S is 2*L+1,
    ppp(size(L->S):count(K))),
  Ks).
  
  
counts_for3(M,Generator,Ks):-
  findall(K,
  (between(0,M,L),
    sols_count(call(Generator,L,_,_),K),S is 2*L+1,
    ppp(size(L->S):count(K))),
  Ks).

dcounts_for2(M,Generator,Ks):-
  findall(K,
  (between(0,M,L),
    sols_count(distinct(call(Generator,L,_)),K),S is 2*L+1,
    ppp(size(L->S):count(K))),
  Ks).
  
  
dcounts_for3(M,Generator,Ks):-
  findall(K,
  (between(0,M,L),
    sols_count(distinct(call(Generator,L,_,_)),K),S is 2*L+1,
    ppp(size(L->S):count(K))),
  Ks).

  
/*
% A262301
?- time(counts_for(7,linear_typed_term,Ks)).
% 8,855,659,045 inferences, 552.730 CPU in 553.015 seconds (100% CPU, 16021680 Lips)
Ks = [1, 3, 26, 367, 7142, 176766, 5304356, 186,954,535, 7,566,084,686].
*/

%:-use_module('third_party/tree_print.pro').

:-include('stats.pro').

show2(N,Gen):-
  call(Gen,N,X),
  ppt(X),
  qqq(X),
  nl,fail;true.

lshow2(N,Gen):-
  call(Gen,N,X),
  %\+ is_linear(X),
  ppt(X),
  to_lambda(X),
  qqq(X),
  ppp('----------------'),
  nl,fail;true.  
  
show3(N,Gen):-
  call(Gen,N,X,T),  
  ppt(X),
  to_lambda(X),qqq(X),
  ppt(T),qqq(T),
  ppp('----------------'),nl,fail;true.

lshow3(N,Gen):-
  call(Gen,N,T,X),
  ppt(X),
  to_lambda(X),qqq(X),
  ppt(T),qqq(T),
  ppp('----------------'),nl,fail;true.
  
g1:-lshow3(3,gen_taut).

g2:-show3(3,linear_typed_normal_form).


%ppp(X):-portray_clause(X).

% counts

gf(N):-counts_for2(N,gen_formula,Ks),ppp('ImplicationalFormulas'(N)=Ks).
gf2(N):-counts_for2(N,gen_formula2,Ks),ppp('ImplicationalFormulas'(N)=Ks).

gt(N):-counts_for3(N,gen_taut,Ks),ppp('LinearTautologies'(N)=Ks).

mc(N):-counts_for2(N,linear_motzkin,Ks),ppp('LinearMotzkin'(N)=Ks).

lc(N):-counts_for2(N,linear_lambda_term,Ks),ppp('LinearLambda'(N)=Ks).

ln(N):-counts_for2(N,linear_normal_form,Ks),ppp('LinearLambda'(N)=Ks).

lt(N):-counts_for3(N,linear_typed_normal_form,Ks),ppp('LinearTermsAndType'(N)=Ks).

go:-gt(7),nl,mc(5),nl,lc(5),nl,ln(5),nl,lt(5).

:-include('polar.pro').

do(G):-G,fail;true.

tgo:-save_traning_set(4).

save_traning_set(M):-
  tell('training.txt'),
  encode_map(M),
  told.

encode_map(M):-
  do((
    between(0,M,N),
    encode_map1(N)
  )).

encode_map1(N):-do((
   linear_typed_normal_form(N,X,T),
   numbervars(X,0,_),
   numbervars(T,0,_),
   encode_term(X,Xs,[]),
   encode_formula(T,Ts,[]),
   maplist(write,Ts),write(':'),maplist(write,Xs),nl
   )).
   
encode_term('$VAR'(I))-->['$VAR'(I)].
encode_term(l(X,E))-->[1],encode_term(X),encode_term(E).
encode_term(a(A,B))-->[0],encode_term(A),encode_term(B).

encode_formula('$VAR'(I))-->['$VAR'(I)].
encode_formula((A '-o' B))-->[0],encode_formula(A),encode_formula(B).


save_dataset(M):-
  do((
    between(0,M,N),
    save_dataset1(N)
  )).
  
save_dataset1(N):-
  make_directory_path('lltaut/'),
  atomic_list_concat(['lltaut/theorems',N,'.pro'],F),
  tell(F), 
  write('% clauses of the form: tp(Theorem,ProofTerm).'),nl,
  write('% preceeded by LaTeX code for Theorem and ProofTerm, as comments'),nl,nl,  
  portray_clause(:-op(900,xfy,('-o'))),nl,
  do((
   linear_typed_normal_form(N,X,T),
   write('% '),qqq(T),
   write('% '),qqq(X),
   portray_clause(tp(T,X))
  )),
  told.
   
sgo:-save_dataset(2).



  
  