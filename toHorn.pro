% reversible translater between implactional
% end embedded Hron clause form

toHorn((A->B),(H:-Bs)):-!,toHorns((A->B),Bs,H).
toHorn(H,H).

toHorns((A->B),[HA|Bs],H):-!,toHorn(A,HA),toHorns(B,Bs,H).
toHorns(H,[],H).  


toAHorn((A->B),(H<-Bs)):-!,toAHorns((A->B),Bs,H).
toAHorn(H,H).

toAHorns((A->B),[HA|Bs],H):-!,toAHorn(A,HA),toAHorns(B,Bs,H).
toAHorns(H,[],H).  


toVarHorn(X,R):-maxvar(X,M),M1 is M+1,functor(D,d,M1),toHorn(X,HBs),toVarHorn1(D,HBs,R).

toVarHorn1(D,(H:-Bs),(VH:-VBs)):-!,maplist(toVarHorn1(D),[H|Bs],[VH|VBs]).
toVarHorn1(D,I,V):-I1 is I+1,arg(I1,D,V).


toEqHorn((A->B),(H:-Bs)):-!,toEqHorns((A->B),Bs,H).
toEqHorn((A<->B),(HA<->HB)):-!,toEqHorn(A,HA),toEqHorn(B,HB).
toEqHorn(H,H).

toEqHorns((A->B),[HA|Bs],H):-!,toEqHorn(A,HA),toEqHorns(B,Bs,H).
toEqHorns((A<->B),[],(HA<->HB)):-!,toEqHorn(A,HA),toEqHorn(B,HB).
toEqHorns(H,[],H).    


toFullHorn((A->B),(H:-Bs)):-!,toFullHorns((A->B),Bs,H).
toFullHorn(H,(H:-[])).

toFullHorns((A->B),[HA|Bs],H):-!,
  toFullHorn(A,HA),
  toFullHorns(B,Bs,H).
toFullHorns(H,[],H).    


toListHorn((A->B),[H|Bs]):-!,toListHorns((A->B),Bs,H).
toListHorn(H,H).

toListHorns((A->B),[HA|Bs],H):-!,
  toListHorn(A,HA),
  toListHorns(B,Bs,H).
toListHorns(H,[],H).    


% same, but the Horn clause bodies are sorted, and 
% clauses like A:-[...,A,...] are trimmed to A:-[A]. 
toSortedHorn((A->B),(H:-Ts)):-!,
   toSortedHorns((A->B),Bs,H),
   sort(Bs,Xs),
   trimHorn(H,Xs,Ts).
   
toSortedHorn(H,H).

toSortedHorns((A->B),[HA|Bs],H):-!,
  toSortedHorn(A,HA),
  toSortedHorns(B,Bs,H).
toSortedHorns(H,[],H).    



% lends the sorting and trimming to implicational form
trimImps(T1,T2):-toSortedHorn(T1,H),toHorn(T2,H).


% shuffles bodies of Horn the enbedded Horn clauses
% for easily running solvers on them in parallel
toRandomHorn((A->B),(H:-Xs)):-!,
   toRandomHorns((A->B),Bs,H),
   trimHorn(H,Bs,Ts),     
   sort(Ts,Cs),
   random_permutation(Cs,Xs).  
toRandomHorn(H,H).

toRandomHorns((A->B),[HA|Bs],H):-!,
  toRandomHorn(A,HA),
  toRandomHorns(B,Bs,H).
toRandomHorns(H,[],H).    

trimHorn(A,Bs,R):-memberchk(A,Bs),!,R=[A].
trimHorn(_,Bs,Bs).


% single threaded randomized run  
rprove(T):-
  toRandomHorn(T,RT),
  hprove(RT).
  
rprove(T,V):-rprove(T),!,V=true.
rprove(_,false).


% creates as many randomized variants
% as number of (useful) availble threads

ranHornPermuted(K,T,Horn):-
  thread_count(K),
  between(1,K,_),
  toRandomHorn(T,Horn).
   
ranPermuted(K,T,RT):-
  ranHornPermuted(K,T,Horn),
  toHorn(RT,Horn).
  
% parallel execution on a set of equivalent
% randomized variants of the initial goal

parProve(T):-parProve(bprove,T).

parProve(P,T):-
 thread_count(K),
 Sol=YesNo,
 ExecGen=ranPermuted(K,T,RT),
 Exec=proveYesNo(P,RT,YesNo),
 nondet_first_with(K,Sol,Exec,ExecGen),
 Sol=true.
 
parProveHorn(T):-parProveHorn(hprove,T).

parProveHorn(P,T):-
 thread_count(K),
 Sol=YesNo,
 ExecGen=ranHornPermuted(K,T,RT),
 Exec=proveYesNo(P,RT,YesNo),
 nondet_first_with(K,Sol,Exec,ExecGen),
 Sol=true.
   
proveYesNo(P,T,YesNo):-
  call(P,T)->YesNo=true
; YesNo=false.
  
% equational forms - a flattening to a Horn clause
% of at most depth 3

hdepth(V,R):-(var(V);atomic(V)),!,R=0.
hdepth((_:-Bs),D):-!,
  maplist(hdepth,Bs,Ds),
  max_list(Ds,M),
  D is M+1.


hsize((_:-Bs),D):-!,
  maplist(hsize,Bs,Ds),
  sum_list(Ds,M),
  D is M+1.
hsize(_,1).
  
% flattens to embedded Horn clauses of depth at most 3


toFlatHorn(A,Horn3):-
  toHorn(A,Horn),
  maxvar(A,M),
  flattenIfNeeded(M,Horn,Horn3).

flattenHorn(Horn,Horn3):-hdepth(Horn,D),D=<3,!,Horn3=Horn.
flattenHorn(Horn,Horn3):-
  limHornVar(Horn,Lim),
  flattenHorn(Lim,Horn,Horn3).
  
limHornVar(A,M):-maxHornVar(A,M0),M is M0+1.

maxHornVar(A,M):-integer(A),!,M=A.
maxHornVar((H:-Bs),M):-
  maplist(maxHornVar,Bs,Ms),
  max_list(Ms,M0),
  M is max(H,M0).

flattenIfNeeded(_,Horn,Horn3):-hdepth(Horn,D),D=<3,!,Horn3=Horn.  
flattenIfNeeded(M,Horn,Horn3):-flattenHorn(M,Horn,Horn3).
  
flattenHorn(M,(H:-Bs),(H:-Fs)):-  
  collect_deep(Bs,Shallow,Deep),
  maplist(flattenDeep,Deep,Heads,Fss,Vss),
  append([Shallow,Heads|Fss],Fs),
  append(Vss,Vs),
  Min is M+1,
  natvars(Min,Vs).

flattenDeep(HBs,(H:-Bs),Fs,Vs):-
  toHorn(A,HBs),
  to_horneq(A,(H:-Bs),Es,[]),
  toFlatEqs(Es,Vs,Fs,[]).
  
collect_deep([],[],[]).
collect_deep([T|Ts],[T|Ls],Rs):-hdepth(T,D),D=<2,!,
  collect_deep(Ts,Ls,Rs).
collect_deep([T|Ts],Ls,[T|Rs]):-
  collect_deep(Ts,Ls,Rs).  

%toFlatHorn(A,FA):-toHorn(A,HA),hdepth(HA,D),D=<3,!,FA=HA.
toFlatHorn1(A,(H:-Fs)):-
  maxvar(A,M),Min is M+1,
  toFlatHornEq(A,H,Fs,Vs),
  natvars(Min,Vs).

toFlatHornEq(A,H,Fs,Vs):-  
 to_horneq(A,(H:-Bs),Es,[]),
 toFlatEqs(Es,Vs,Fs,Bs).

toFlatEqs([],[])-->[].
toFlatEqs([V=(H:-Bs)|Es],[V|Vs])-->
  [V:-[H:-Bs]],
  [(H:-[V|Bs])],
  toFlatEqs(Es,Vs).
 
%%%  
  
toHornEq(A,(H:-Es)):- 
  to_horneq(A,(H:-Bs),Es,Bs),
  maxvar(A,M),Min is M+1,natvars(Min,Es).
  
to_horneq(A, HBs,Es):-to_horneq(A,HBs,Es,[]).

to_horneq(A,H)-->{atomic(A);var(A)},!,{H=A}.
to_horneq(A,(H:-Bs))-->to_horneqs(A,Bs,H).

to_horneqs(A,[],H)-->{atomic(A);var(A)},!,to_horneq(A,H).
to_horneqs((A->B),[R|Bs],H)-->!,
   to_horneq(A,HA),
   to_eq(HA,R),
   to_horneqs(B,Bs,H).


to_eq(HA,R)--> {atomic(HA);var(HA)},
  !,
  {R=HA}.
  to_eq(HA,V)-->[V=HA].
  
  

toFlatImp(A,B):-
  toFlatHorn(A,F),
  toHorn(B,F).
  
  
horn2term(N,A):-atomic(N),!,to_atom(N,A).
horn2term((H:-Bs),T):-
   maplist(horn2term,Bs,As),
   to_atom(H,F),
   T=..[F|As].

term2horn(T,H):-atomic(T),!,H=T.
term2horn(T,(F:-Bs)):-
  T=..[F|Xs],
  maplist(term2horn,Xs,Bs).
   
to_atom(A,R):-atom(A),!,R=A.
to_atom(N,R):-integer(N),atom_number(R,N).

imp2pro-->toHorn,horn2term.

fromHorn(A,B):-toHorn(B,A).

pro2imp-->term2horn,fromHorn.


%%%%%%%%%

toNestedHorn(A,R):-
  %ppp(orig=A),
  expand_equiv(A,X),
  %ppp(equiv=X),
  toHorn1(X,H),
  %ppp(horn=H),
  expand_horn(H,E),
  %ppp(exp=E),
  reduce_heads(E,R),
  %ppp(red=R),nl,
  true.
  
primitive(A):-atomic(A).

expand_equiv(A,R):-primitive(A),!,R=A.
expand_equiv(~ ~ ~ A,X):-!,expand_equiv(~ A,X).
expand_equiv(~ A,(X->false)):-!,expand_equiv(A,X).
expand_equiv(A<->A,R):-!,R=true.
expand_equiv(A<->B,(X->Y)&(Y->X)):-!,
  expand_equiv(A,X),
  expand_equiv(B,Y).
expand_equiv(A,R):-A=..[F|Xs],
  maplist(expand_equiv,Xs,Ys),
  R=..[F|Ys].
  
expand_horn(A,R):-primitive(A),!,R=A.
expand_horn(A&A,R):-!,expand_horn(A,R).
expand_horn(A&B,Xs):-!,listify(A&B,Xss),flatten_it(Xss,Xs).
expand_horn((H:-Bs),HBss):-
  expand_horn(H,HH),flatten_it(HH,Hs),
  maplist(expand_horn,Bs,Css),
  flatten_it(Css,Cs),
  %ppp(hs+Cs=Hs+Cs),
  distribute_head(Hs,Cs,HBss).

%listify(A,_):-ppp(listify:A),fail.
listify(A&B,[X|Xs]):-!,expand_horn(A,X),listify(B,Xs).
listify(A,[X]):-expand_horn(A,X).

distribute_head([],_,[]).
distribute_head([H|Hs],Bs,[(H:-Bs)|HBss]):-distribute_head(Hs,Bs,HBss).

reduce_heads(Bs,Cs):-is_list(Bs),!,reduce_body(Bs,Cs).
reduce_heads(H,R):-reduce_head(H,R).

%reduce_head(H,_R):-ppp(reduce_head(H)),fail.
reduce_head(H,R):-primitive(H),!,R=H.
reduce_head((H:-Bs),(H:-Cs)):-primitive(H),!,
  reduce_body(Bs,Us),
  sort(Us,Cs).
reduce_head((CH:-Bs),(H:-Rs)):-
  reduce_head(CH,(H:-Cs)),
  reduce_body(Bs,Ds),
  append(Cs,Ds,Us),
  sort(Us,Rs).

  
reduce_body([],[]).
reduce_body([B|Bs],[C|Cs]):-
  reduce_head(B,C),
  reduce_body(Bs,Cs).


flatten_it(Xs,Fs):-list:flatten(Xs,Fs).  

toHorn1((A&A),R):-!,toHorn1(A,R).
toHorn1((A&B),X&Y):-!,toHorn1(A,X),toHorn1(B,Y).
toHorn1((A->A),R):-!,R=true.
toHorn1((A->B),(H:-Bs)):-!,toHorns1((A->B),Bs,H).
toHorn1(H,H).

toHorns1((A->B),[HA|Bs],H):-!,toHorn1(A,HA),toHorns1(B,Bs,H).
toHorns1(H,[],HH):-toHorn1(H,HH).
