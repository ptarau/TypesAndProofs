% discovery of epistemic and alethic logic extensions
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
expand_defs(D,* A,R):-!,expand_defs(D,~ (# (~ (A))),R).

expand_defs(D,#(X),R) :-!,copy_term(D,(#(X):-T)),
  %ppp(D+here(X)=T),
  expand_defs(D,T,R).
expand_defs(D,A,B) :-
  A=..[F|Xs],
  expand_def_list(D,Xs,Ys),
  B=..[F|Ys].


expand_def_list(_,[],[]).
expand_def_list(D,[X|Xs],[Y|Ys]) :-expand_defs(D,X,Y),expand_def_list(D,Xs,Ys).

iel_prove(D,T0) :-
  %ppp(t0=T0),
  expand_defs(D,T0,T1),
  %ppp(t1=T1),
  ljfa(T1,[]).
  
def_synth(M,D):-def_synth(M,iel_th,iel_nth,D).

def_synth(M,Th,NTh,D):-
  genDef(M,D),
  %ppp(D),
  forall(call(Th,T),iel_prove(D,T)),
  %forall(iel_th(T),iel_prove(D,#T)), % necessitation rule would fail
  forall(call(NTh,NT), \+iel_prove(D,NT)).


  
  
% IEL embedding

% IEL- axioms
iel_th(a -> # a).
iel_th(# (a->b)->(# a-> # b)).
iel_th(# p <-> # # p).

% IEL axiom
iel_th(# a -> ~ ~ a).

% some theorems
iel_th(#   (a & b) <-> (# a & # b)).
iel_th(~ # false).
iel_th(~ (# a & ~ a)).
iel_th(~a -> ~ # a).
iel_th( ~ ~ (# a -> a)).

% some other
iel_th(# a & # (a->b) -> # b).
iel_th(# a -> ~ # (~ a)).

iel_th(* (a & b) <-> (* a & * b)).

iel_th(# a -> * a).
iel_th(# a v # b -> # (a v b) ).
iel_th(* a <-> * * a).

% should fail
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

% S4 embedding

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

% discovery of IEL formula definitions
iel_discover:-
   do((def_synth(2,iel_th,iel_nth,D),ppp(D))).
 
iel_nec_discover:-
  do((def_synth(2,iel_nec_th,iel_nth,D),ppp(D))).   
   
% test of the most interesting descovered formula 
iel_test:-
   %Def=(#X :- X & (?)),
   Def=(#A:-(A->(?))->A),
   ppp('theorems'),
   do((
     iel_th(T),write(T),
     %expand_defs(Def,T,ET),write('==>'),write(ET),
     (iel_prove(Def,T)->write(' :: expected to be PROVEN');true),
     nl
   )),
   nl,
   ppp('necessitation rule: if proven A then #A'),
   do((
     iel_th(T0),T= #T0,write(T),
     %expand_defs(Def,T,ET),write('==>'),write(ET),
     (iel_prove(Def,T)->write(' :: expected to be PROVEN');true),
     nl
     )),
   nl,
   ppp('non-theorems'),
   do((
     iel_nth(T),write(T),
     %expand_defs(Def,T,ET),write('==>'),write(ET),
     (\+iel_prove(Def,T)->write(' :: proof expected to FAIL');true),
     nl
   )).
   
s4_discover:-
  do((def_synth(2,s4_th,s4_nth,D),ppp(D))).   
  
s4_nec_discover:-
  do((def_synth(3,s4_nec_th,s4_nth,D),ppp(D))).   
   
   
   
   
   
   
   
   
   