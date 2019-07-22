
:- op( 500,  fy, #).   
:- op( 500,  fy, *).


genDef(Def):-genDef(2,Def).

genDef(M,Def):-genDef(M,[->,&,v],[false,?],Def).

genDef(M,Ops,Cs,(#(X):-T)):-
  between(0,M,N),
  genOpTree(N,Ops,T,Vs),
  pick_leaves(Vs,[X|Cs]),
  term_variables(Vs,[X]).
   
pick_leaves([],_).
pick_leaves([V|Vs],Ls):-member(V,Ls),pick_leaves(Vs,Ls).

genModForms(M,T):-
  between(0,M,N),
  genOpTree(N,[(#)/1,(*)/1,&,v,->,<->,~],T,Vs),
  natvars(Vs).

expand_defs(_,false,R) :-!,R=false.
expand_defs(_,true,R) :-!,R=true.
expand_defs(_,A,R) :-atomic(A),!,R= A.
expand_defs(D,~(A),(B->false)) :-!,expand_defs(D,A,B).
expand_defs(D,* A,R):-!,expand_defs(D,~ (# (~ A)),R).

expand_defs(D,#(X),R) :-!,copy_term(D,(#(X):-T)),
  %ppp(D+here(X)=T),
  expand_defs(D,T,R).
expand_defs(D,A,B) :-
  A=..[F|Xs],
  expand_def_list(D,Xs,Ys),
  B=..[F|Ys].


expand_def_list(_,[],[]).
expand_def_list(D,[X|Xs],[Y|Ys]) :-expand_defs(D,X,Y),expand_def_list(D,Xs,Ys).

kaprove(D,T0) :-
  %ppp(t0=T0),
  expand_defs(D,T0,T1),
  %ppp(t1=T1),
  ljfa(T1,[]).
  
k_synt(D):-
  genDef(D),
  %ppp(D),
  forall(thk(T),kaprove(D,T)),
  %forall(thk(T),kaprove(D,#T)), % necessitation rule would fail
  forall(nthk(NT), \+kaprove(D,NT)).

/* assuming an S4 prover modprove
m_synt(D):-
  genDef(D),
  %ppp(D),
  forall(genModForms(2,T),kaprove(D,T)),
  forall(nthk(NT), \+kaprove(D,NT)).
  
gen_mod_th(M,T):-genModForms(M,T),
  ppp(T),
  modprove(T).
*/

/*
thk(# a -> a).
thk(# (a->b) -> (# a -> # b)). 
thk(# a <-> # # a).
thk(* * a <-> * a).
thk(a -> * a).
thk(# a -> * a).

thk(# a v # b -> # (a v b)). 
thk(# (a v b) -> # a v # b). 

nthk(# a).
nthk(~ (# a)).
nthk(# false).
nthk(* false).
nthk(* a -> # * a).
nthk(a -> # a).
nthk(* a -> a).
nthk(# a <-> ?).
nthk(* a <-> ?).
*/
  
% some theorems
% IEL- axioms
thk(a -> # a).
thk(# (a->b)->(# a-> # b)).
thk(# p <-> # # p).

% IEL axiom
thk(# a -> ~ ~ a).

% some theorems
thk(#   (a & b) <-> (# a & # b)).
thk(~ # false).
thk(~ (# a & ~ a)).
thk(~a -> ~ # a).
thk( ~ ~ (# a -> a)).

% some other
thk(# a & # (a->b) -> # b).
thk(# a -> ~ # (~ a)).

thk(* (a & b) <-> (* a & * b)).

thk(# a -> * a).
thk(# a v # b -> # (a v b) ).
thk(* a <-> * * a).

% should fail
nthk(# a -> a).
nthk(# (a v b) -> # a v # b).

nthk(# a).
nthk(~ (# a)).
nthk(# false).
nthk(# a).
nthk(~ (# a)).
nthk(* false).

kgo:-
   %Def=(#X :- X & (?)),
   Def=(#A:-(A->(?))->A),
   ppp('theorems'),
   do((
     thk(T),write(T),
     %expand_defs(Def,T,ET),write('==>'),write(ET),
     (kaprove(Def,T)->write(' :: expected to be PROVEN');true),
     nl
   )),
   nl,
   ppp('necessitation rule: if proven A then #A'),
   do((
     thk(T0),T= #T0,write(T),
     %expand_defs(Def,T,ET),write('==>'),write(ET),
     (kaprove(Def,T)->write(' :: expected to be PROVEN');true),
     nl
     )),
   nl,
   ppp('non-theorems'),
   do((
     nthk(T),write(T),
     %expand_defs(Def,T,ET),write('==>'),write(ET),
     (\+kaprove(Def,T)->write(' :: proof expected to FAIL');true),
     nl
   )).
   
   
   
   
   
   
   
   
   
   
   