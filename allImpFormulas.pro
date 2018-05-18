% all implicational logic formulas of size N
allImpFormulas(N,T):-
  genTree(N,T,Vs),
  vpartitions(Vs),
  natvars(Vs).
 
% all classical implicational formulas  
allClassFormulas(N,T):-
  genTree(N,T,Vs),
  vpartitions(Vs),
  classvars(Vs).
  
% all Glivenko tranformed classic formulas  
allClassFormulas(N,T,NNT):-
  genTree(N,T,Vs),
  vpartitions(Vs),
  dneg(T,NNT),
  natvars(Vs).
 
 
/* 

A289679: Catalan*Bell

?- findall(S,(between(0,7,N),sols(allImpFormulas(N,_),S)),Xs).
Xs = [1, 2, 10, 75, 728, 8526, 115764, 1776060].

*/

seqCountProvenFormulas(N,P,ProvenCount,TotalCount):-
  seqCountProvenFormulas(_,N,P,ProvenCount,TotalCount).

seqCountProvenFormulas(I/M,N,P,ProvenCount,TotalCount):-
  new_ctr(Total),new_ctr(Proven),
  do((
    nthImpFormula(N,T,K),
    (integer(I)->I=:=K mod M;true),
     ctr_inc(Total),
     call(P,T),
     ctr_inc(Proven)
  )),
  ctr_get(Proven,ProvenCount),
  ctr_get(Total,TotalCount).

nthImpFormula(N,T,K):-
  new_ctr(C),
  allImpFormulas(N,T),
  ctr_get(C,K),
  ctr_inc(C).
  
parCountProvenFormulas(N,P,Proven,Total):-
  thread_count(M0),M is M0+1,
  G=seqCountProvenFormulas(I/M,N,P,_,_),
  findall(G,between(0,M,I),Gs),
  concurrent(M0,Gs,[]),
  maplist(arg(4),Gs,Ps),sum_list(Ps,Proven),
  maplist(arg(5),Gs,Ts),sum_list(Ts,Total).

  
parCountProvenFormulas1(N,P,Proven,Total):-
  init(total_imp),
  nondet_count(call(P,T),gen_and_count(N,T),Proven),
  total(total_imp,Total).
  
gen_and_count(N,T):-
  allImpFormulas(N,T),
  inc(total_imp).
  
gen_and_count_and_prove1(N,P):-
  allImpFormulas(N,T),
  inc(total_imp),
  prove(P,T),
  inc(total_proven).
  
  
/*

?- time(parCountProvenFormulas(8,hprove,A,B)).
% 32,990 inferences, 0.010 CPU in 200.569 seconds (0% CPU, 3356394 Lips)
A = 6215192,
B = 30240210.

?- findall(A+B,(between(1,8,N),parCountProvenFormulas(N,hprove,A,B)),R).
R = [1+2, 3+10, 24+75, 201+728, 2201+8526, 27406+115764, 391379+1776060].
*/
  