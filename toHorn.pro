% reversible tranlater between implactional
% end embedded Hron clause form

toHorn((A->B),(H:-Bs)):-!,toHorns((A->B),Bs,H).
toHorn(H,H).

toHorns((A->B),[HA|Bs],H):-!,toHorn(A,HA),toHorns(B,Bs,H).
toHorns(H,[],H).    



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
% as number of (useful) availbel threads

ranHornPermuted(K,T,Horn):-
  thread_count(K),
  between(1,K,_),
  toRandomHorn(T,Horn).
   
ranPermuted(K,T,RT):-
  ranHornPermuted(K,T,Horn),
  toHorn(RT,Horn).
  
% parallel execution on a set of equivalent
% randomized variants of the initial goal

parProve(T):-parProve(pprove,T).

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
  
