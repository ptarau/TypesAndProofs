
toHorn((A->B),(H:-Bs)):-!,toHorns((A->B),Bs,H).
toHorn(H,H).

toHorns((A->B),[HA|Bs],H):-!,toHorn(A,HA),toHorns(B,Bs,H).
toHorns(H,[],H).    


toSortedHorn((A->B),(H:-Ts)):-!,
   toSortedHorns((A->B),Bs,H),
   revsort(Bs,Xs),
   trimHorn(H,Xs,Ts).
   
toSortedHorn(H,H).

toSortedHorns((A->B),[HA|Bs],H):-!,
  toSortedHorn(A,HA),
  toSortedHorns(B,Bs,H).
toSortedHorns(H,[],H).    


trimHorn(A,Bs,R):-memberchk(A,Bs),!,R=[A].
trimHorn(_,Bs,Bs).


trimImps(T1,T2):-toSortedHorn(T1,H),toHorn(T2,H).



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

randomize(ImpTerm,R):-
  toRandomHorn(ImpTerm,Horn),
  toHorn(R,Horn).

  
rprove(T):-
  randomize(T,RT),
  %ppp(RT),
  pprove(RT).

  
rprove(T,V):-rprove(T),!,V=true.
rprove(_,false).

pprove(T,V):-pprove(T),!,V=true.
pprove(_,false).

ranPermuted(K,T,RT):-
  thread_count(K),
  between(1,K,_),
  randomize(T,RT).
  

rrprove(T):-
 thread_count(K),
 Sol=YesNo,
 ExecGen=ranPermuted(K,T,RT),
 Exec=pprove(RT,YesNo),
 nondet_first_with(K,Sol,Exec,ExecGen),
 Sol=true.
   
 