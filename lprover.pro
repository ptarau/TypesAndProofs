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

all proof terms are closed lambda terms
most proof terms for small formulas are affine
very few among the affine are linear
no linear implicational intuitionistic tautologies of even size
*/

generate_linear(N,T,Proof):-allImpFormulas(N,T),prove_linear(Proof,T).

generate_affine(N,T,Proof):-allImpFormulas(N,T),prove_affine(Proof,T).

generate_intuitionist(N,T,Proof):-allImpFormulas(N,T),prove_ipc(Proof,T).

prove_linear(X,T):-prove_ipc(X,T),is_linear(X).

prove_affine(X,T):-prove_ipc(X,T),is_affine(X).

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

head_of(_->B,G):-!,head_of(B,G).
head_of(G,G). 

vars_of(T,Vs,Xs):-vars_of(T,Vs,[],Xs,[]).

vars_of(V,[V|Vs],Vs)-->{var(V)},!.
vars_of(l(K,X),Vs,Us)-->[K],vars_of(X,Vs,Us).
vars_of(a(X,Y),Vs1,Vs3)-->vars_of(X,Vs1,Vs2),vars_of(Y,Vs2,Vs3).

is_closed(X):-
  \+ \+ (
    vars_of(X,Vs,Bs),
    numbervars(Bs,0,_),
    ground(Vs)
  ).

is_affine(LambdaTerm):-
  vars_of(LambdaTerm,Vars,_Binders),
  sort(Vars,Us),
  length(Vars,LenAll),
  length(Us,LenUniques),
  LenUniques =:= LenAll.

is_linear(LambdaTerm):-
  vars_of(LambdaTerm,Vars,Binders),
  sort(Vars,Us),
  length(Vars,LenAll),
  length(Us,LenUniques),
  LenUniques =:= LenAll,
  sort(Binders,Sorted),
  Sorted==Us.

% all implicational logic formulas of size N
% A289679		a(n) = Catalan(n-1)*Bell(n).
allImpFormulas(N,T):-
  genTree(N,T,Vs),
  natpartitions(Vs).
  
genTree(N,Tree,Leaves):-genTree(Tree,N,0,Leaves,[]).

genTree(V,N,N)-->[V].
genTree((A->B),SN1,N3)-->{SN1>0,N1 is SN1-1},
  genTree(A,N1,N2),
  genTree(B,N2,N3).
  
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

% tools

ppp(X):-numbervars(X,0,_),writeln(X);fail.

% stats

% counts nb. of solutions of Goal 
sols(Goal, Times) :-
        Counter = counter(0),
        (   Goal,
            arg(1, Counter, N0),
            N is N0 + 1,
            nb_setarg(1, Counter, N),
            fail
        ;   arg(1, Counter, Times)
        ).

counts_for(M,Generator,Ks):-
  findall(K,(between(0,M,L),sols(call(Generator,L,_,_),K)),Ks).
  
% count linear, affine, and intuitionistic tautologies of up to size 7

lin_counts(Ks):-counts_for(7,generate_linear,Ks).  
aff_counts(Ks):-counts_for(7,generate_affine,Ks).
intuit_counts(Ks):-counts_for(7,generate_intuitionist,Ks).

go:-generate_linear(5,T,Proof),ppp(formula=T),ppp(proof_term=Proof),nl,fail;true.
