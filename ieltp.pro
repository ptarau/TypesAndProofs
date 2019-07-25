:- op(525,  fy,  ~ ).
:- op(550, xfy,  & ).    % right associative
:- op(575, xfy,  v ).    % right associative
:- op(600, xfx,  <-> ).  % non associative

prove_in_ipc(T):-  prove_in_ipc(T,[]).

prove_in_ipc(A,Vs):-memberchk(A,Vs),!.
prove_in_ipc(_,Vs):-memberchk(false,Vs),!.
prove_in_ipc(A<->B,Vs):-!,prove_in_ipc(B,[A|Vs]),prove_in_ipc(A,[B|Vs]).
prove_in_ipc((A->B),Vs):-!,prove_in_ipc(B,[A|Vs]).
prove_in_ipc(A & B,Vs):-!,prove_in_ipc(A,Vs),prove_in_ipc(B,Vs).
prove_in_ipc(G,Vs1):- % atomic or disj or false
  select(Red,Vs1,Vs2),
  prove_in_ipc_reduce(Red,G,Vs2,Vs3),
  !,
  prove_in_ipc(G,Vs3).
prove_in_ipc(A v B, Vs):-(prove_in_ipc(A,Vs);prove_in_ipc(B,Vs)),!.

prove_in_ipc_reduce((A->B),_,Vs1,Vs2):-!,prove_in_ipc_imp(A,B,Vs1,Vs2).
prove_in_ipc_reduce((A & B),_,Vs,[A,B|Vs]):-!.
prove_in_ipc_reduce((A<->B),_,Vs,[(A->B),(B->A)|Vs]):-!.
prove_in_ipc_reduce((A v B),G,Vs,[B|Vs]):-prove_in_ipc(G,[A|Vs]).
  
prove_in_ipc_imp((C->D),B,Vs,[B|Vs]):-!,prove_in_ipc((C->D),[(D->B)|Vs]).
prove_in_ipc_imp((C & D),B,Vs,[(C->(D->B))|Vs]):-!.
prove_in_ipc_imp((C v D),B,Vs,[(C->B),(D->B)|Vs]):-!.
prove_in_ipc_imp((C<->D),B,Vs,[((C->D)->((D->C)->B))|Vs]):-!.
prove_in_ipc_imp(A,B,Vs,[B|Vs]):-memberchk(A,Vs).  

genOperatorTree(N,Ops,Tree,Leaves):-
  genOperatorTree(Ops,Tree,N,0,Leaves,[]).
    
genOperatorTree(_,V,N,N)-->[V].
genOperatorTree(Ops,OpAB,SN1,N3)-->
  { SN1>0,N1 is SN1-1,
    member(Op,Ops),make_oper2(Op,A,B,OpAB)
  },
  genOperatorTree(Ops,A,N1,N2),
  genOperatorTree(Ops,B,N2,N3).
  
make_oper2(Op,A,B,OpAB):-functor(OpAB,Op,2),arg(1,OpAB,A),arg(2,OpAB,B).

:- op( 500,  fy, #).   
:- op( 500,  fy, *).

genDef(M,Def):-genDef(M,[(->),(&),(v)],[false,?],Def).

genDef(M,Ops,Cs,(#(X):-T)):-
  between(0,M,N),
  genOperatorTree(N,Ops,T,Vs),
  pick_leaves(Vs,[X|Cs]),
  term_variables(Vs,[X]).

pick_leaves([],_).
pick_leaves([V|Vs],Ls):-member(V,Ls),pick_leaves(Vs,Ls).

expand_defs(_,false,R) :-!,R=false.
expand_defs(_,A,R) :-atomic(A),!,R= A.
expand_defs(D,~(A),(B->false)) :-!,expand_defs(D,A,B).
expand_defs(D,*(A),R):-!,expand_defs(D,~ (# (~(A))),R).

expand_defs(D,#(X),R) :-!,copy_term(D,(#(X):-T)),expand_defs(D,T,R).

expand_defs(D,A,B) :-
  A=..[F|Xs],
  expand_def_list(D,Xs,Ys),
  B=..[F|Ys].

expand_def_list(_,[],[]).
expand_def_list(D,[X|Xs],[Y|Ys]) :-
  expand_defs(D,X,Y),
  expand_def_list(D,Xs,Ys).

prove_with_def(Def,T0) :-expand_defs(Def,T0,T1),prove_in_ipc(T1,[]).

def_synth(M,D):-def_synth(M,iel_th,iel_nth,D).

def_synth(M,Th,NTh,D):-
  genDef(M,D),
  forall(call(Th,T),prove_with_def(D,T)),
  forall(call(NTh,NT), \+prove_with_def(D,NT)).

iel_th(a -> # a).
iel_th(# (a->b)->(# a-> # b)).
iel_th(# p <-> # # p).
iel_th(# a -> ~ ~ a).

iel_th(#   (a & b) <-> (# a & # b)).
iel_th(~ # false).
iel_th(~ (# a & ~ a)).
iel_th(~a -> ~ # a).
iel_th( ~ ~ (# a -> a)).
iel_th(# a & # (a->b) -> # b).
iel_th(* (a & b) <-> (* a & * b)).
iel_th(# a -> * a).
iel_th(# a v # b -> # (a v b) ).
iel_th(* a <-> * * a).
iel_th(a -> *a).

iel_nth(# a -> a).
iel_nth(# (a v b) -> # a v # b).
iel_nth(# a).
iel_nth(~ (# a)).
iel_nth(# false).
iel_nth(# a).
iel_nth(~ (# a)).
iel_nth(* false).

iel_nec_th(T):-iel_th(T).
iel_nec_th(# T):-iel_th(T).

iel_discover:-
  backtrack_over((def_synth(2,iel_th,iel_nth,D),println(D))).
 
iel_nec_discover:-
  backtrack_over((def_synth(2,iel_nec_th,iel_nth,D),println(D))).
  
backtrack_over(Goal):-call(Goal),fail;true. 

println(T):-numbervars(T,0,_),writeln(T). 

% test of the most interesting discovered formula 
iel_test:-
   %Def=(#X :- X & ?),
   Def=(#A:-(A -> ?) -> A),
   println('theorems'),
   backtrack_over((
     iel_th(T),
     (prove_with_def(Def,T)->Mes=' expected to be PROVEN';Mes='failed'),
     println(T : Mes)
   )),nl,
   println('necessitation rule: if proven A then #A'),
   backtrack_over((
     iel_th(T0),T= #T0,
     (prove_with_def(Def,T)->Mes=' expected to be PROVEN';Mes='failed'),
     println(T : Mes) 
   )),nl,
   println('non-theorems'),
   backtrack_over((
     iel_nth(T),
     (\+ prove_with_def(Def,T)->Mes=' expected to FAIL';Mes='wrongly proven'),
     println(T:Mes)
   )),nl.

iel_prove(P):-prove_with_def((#A :- (A -> eureka) -> A),P).

s4_th(# a -> a).
s4_th(# (a->b) -> (# a -> # b)). 
s4_th(# a <-> # # a).
s4_th(* * a <-> * a).

s4_th(a -> * a).
s4_th(# a -> * a).
s4_th(# a v # b -> # (a v b)). 
s4_th(# (a v b) -> # a v # b). 

s4_nth(# a).
s4_nth(~ (# a)).
s4_nth(# false).
s4_nth(* false).
s4_nth(* a -> # * a).
s4_nth(a -> # a).
s4_nth(* a -> a).
s4_nth(# a <-> ?).
s4_nth(* a <-> ?).

s4_nec_th(T):-s4_th(T).
s4_nec_th(# T):-s4_th(T).

s4_discover:-
  backtrack_over((def_synth(2,s4_th,s4_nth,D),println(D))).   
  
s4_nec_discover:-
  backtrack_over((def_synth(2,s4_nec_th,s4_nth,D),println(D))). 

