% all implicational logic formulas of size N
allHornFormulas(N,T):-
  succ(N,SN),length(Vs,SN),
  natpartitions(Vs),
  genHorn(N,T,Vs).

% all Horn formulas with bodies in canonical order
% to break symmetries irrelevant for testing provers
allSortedHorn(N,T):-
  succ(N,SN),length(Vs,SN),
  natpartitions(Vs),
  genSortedHorn(N,T,Vs).

countSortedHorn(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allSortedHorn,hprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).

  
% all Horn formulas with bodies in canonical order
% to break symmetries irrelevant for testing provers
% of depth at most 3, as deeper ones can be reduced to these

allSortedHorn3(N,T):-
  succ(N,SN),length(Vs,SN),
  natpartitions(Vs),
  genSortedHorn3(N,T,Vs).

countSortedHorn3(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allSortedHorn3,hprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).

countSortedHorn_alt(M):-ncounts(M,allSortedHorn(_,_)).
  
gen_and_count(N,G,P,[proven=Proven,total=Total,ratio=Ratio]):-
  init(total_count),
  init(proven_count),
  do((
    call(G,N,T),
    inc(total_count),
    call(P,T),
    inc(proven_count)
  )),
  total(total_count,Total),
  total(proven_count,Proven),
  R is Proven/Total,
  nice_num(R,Ratio).

  
% all implicational logic formulas of size N
allImpFormulas(N,T):-
  genTree(N,T,Vs),
  natpartitions(Vs).
 
countAllImp(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allImpFormulas,hprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).  
  
  
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

allEqFormulas(N,T):-
  genOpTree(N, [(->),(<->)], T, Vs),
  natpartitions(Vs).
  

allFullFormulas(N,T):-
  genOpTree(N,T,Vs),
  natpartitions(Vs).

countAllFull(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allFullFormulas,faprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).  

countAllFullDyckhoff(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allFullFormulas,dprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).  

  
/*

?- countAllFull(9).
[proven=0,total=1,ratio=0]
[proven=0,total=1,ratio=0]
[proven=2,total=10,ratio=0.2]
[proven=0,total=26,ratio=0]
[proven=19,total=283,ratio=0.067]
[proven=21,total=1488,ratio=0.014]
[proven=497,total=17626,ratio=0.028]
[proven=1317,total=173636,ratio=0.007]
[proven=20693,total=2510404,ratio=0.008]
true.

!!! discrepancy

?- countAllFullDyckhoff(9).
[proven=0,total=1,ratio=0]
[proven=0,total=1,ratio=0]
[proven=2,total=10,ratio=0.2]
[proven=0,total=26,ratio=0]
[proven=29,total=283,ratio=0.102]
[proven=41,total=1488,ratio=0.027]
[proven=887,total=17626,ratio=0.05]
[proven=3168,total=173636,ratio=0.018]
[proven=46798,total=2510404,ratio=0.018]
*/

seqCountImpFormulas(N,TotalCount):-
  seqCountImpFormulas(_,N,TotalCount).

seqCountImpFormulas(I/M,N,TotalCount):-
  new_ctr(Total),
  do((
    nthImpFormula(N,_T,K),
    (integer(I)->I=:=K mod M;true),
     ctr_inc(Total)
  )),
  ctr_get(Total,TotalCount).

% just to compare the overhead for provers  
parCountImpFormulas(N,Total):-
  thread_count(M1),M0 is M1-1,
  G=seqCountImpFormulas(I/M1,N,_),
  findall(G,between(0,M0,I),Gs),
  concurrent(M1,Gs,[]),
  maplist(arg(3),Gs,Ts),sum_list(Ts,Total).
  
  
/* 

A289679: Catalan(n)*Bell(n+1)

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
  thread_count(M1),M0 is M1-1,
  G=seqCountProvenFormulas(I/M1,N,P,_,_),
  findall(G,between(0,M0,I),Gs),
  concurrent(M1,Gs,[]),
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


% some stats

itautVSforms(
  [0/1,1/2, 3/10, 24/75, 201/728, 2201/8526, 27406/115764, 391379/1776060, 6215192/30240210 , 108722929/563870450]).

ctautVSforms([0/1,1/2, 3/10, 25/75, 206/728, 2298/8526, 28504/115764, 409543/1776060, 6504136/30240210,114013199/563870450]).
   
itratio:-tratio(itautVSforms).
ctratio:-tratio(ctautVSforms).

icratio:-itautVSforms(Is),ctautVSforms(Cs),
  maplist(safe_div,Is,Cs,Rs),
  ppp(Rs),
  plotl(Rs).

safe_div(_,0/_,R):-!,R=undef.
safe_div(X,Y,Z):-Z is X/Y.

tratio(D):-
  call(D,Ps),
  maplist(is,Xs,Ps),
  maplist(nice_num,Xs,Qs),
  ppp(Qs),
  plotl(Qs).
  

unInhabitableTree(N,T):-
  genSortedHorn(N,T,Vs),
  \+ (
    natpartitions(Vs),
    hprove(T)
  ).
  
unInhabitableVars(N,Vs):-N>0,
  N1 is N-1,
  vpartitions(N,Vs),natvars(Vs),
  \+ (
    genSortedHorn(N1,T,Vs),
    hprove(T)
  ).  
    
    
% [1,0,1,1,4,7,23,53,163]
countUnInhabitableTree(M):-ncounts(M,unInhabitableTree(_,_)).

% [0,1,1,4,9,30,122,528,2517,12951]
countUnInhabitableVars(M):-ncounts(M,unInhabitableVars(_,_)).


  
  
/*
?- time(seqCountProvenFormulas(8,hprove,A,B)).
% 2,243,311,670 inferences, 217.446 CPU in 217.577 seconds (100% CPU, 10316659 Lips)
A = 6215192,
B = 30240210.

?- time(parCountProvenFormulas(8,hprove,A,B)).
% 32,990 inferences, 0.010 CPU in 200.569 seconds (0% CPU, 3356394 Lips)
A = 6215192,
B = 30240210.

?- findall(A/B,(between(0,9,N),parCountProvenFormulas(N,hprove,A,B)),R).
R = [0/1,1/2, 3/10, 24/75, 201/728, 2201/8526, 27406/115764, 391379/1776060, 6215192/30240210  108722929/563870450].

?- time(parCountProvenFormulas(9,hprove,A,B)).
% 1,121 inferences, 0.075 CPU in 4008.850 seconds (0% CPU, 14852 Lips)
A = 108722929,
B = 563870450.

?- findall(A/B,(between(0,9,N),parCountProvenFormulas(N,tautology,A,B)),R).
R = [0/1,1/2, 3/10, 25/75, 206/728, 2298/8526, 28504/115764, 409543/1776060, 6504136/30240210,114013199/563870450].

- itratio.
[0,0.5,0.3,0.32,0.27,0.25,0.23,0.22,0.2,0.19]
x,y
0,0
1,0.5
2,0.3
3,0.32
4,0.27
5,0.25
6,0.23
7,0.22
8,0.2
9,0.19

true.

?- ctratio.
[0,0.5,0.3,0.33,0.28,0.26,0.24,0.23,0.21,0.2]
x,y
0,0
1,0.5
2,0.3
3,0.33
4,0.28
5,0.26
6,0.24
7,0.23
8,0.21
9,0.2

true.

?- icratio.
[undef,1.0,1.0,0.9600000000000001,0.975728155339806,0.9577893820713663,0.9614790906539433,0.9556481248611258,0.9555753446729897,0.9535994950900377]
x,y
1,1.0
2,1.0
3,0.9600000000000001
4,0.975728155339806
5,0.9577893820713663
6,0.9614790906539433
7,0.9556481248611258
8,0.9555753446729897
9,0.9535994950900377

true.

?- countSortedHorn(9).
[proven=0,total=1,ratio=0]
[proven=1,total=2,ratio=0.5]
[proven=1,total=7,ratio=0.142]
[proven=9,total=38,ratio=0.236]
[proven=42,total=266,ratio=0.157]
[proven=335,total=2263,ratio=0.148]
[proven=2772,total=22300,ratio=0.124]
[proven=27699,total=247737,ratio=0.111]
[proven=303645,total=3049928,ratio=0.099]
true.

?- countSortedHorn3(9).
[proven=0,total=1,ratio=0]
[proven=1,total=2,ratio=0.5]
[proven=1,total=7,ratio=0.142]
[proven=9,total=38,ratio=0.236]
[proven=42,total=214,ratio=0.196]
[proven=329,total=1977,ratio=0.166]
[proven=2438,total=16004,ratio=0.152]
[proven=24342,total=174377,ratio=0.139]
[proven=240323,total=1876093,ratio=0.128]
true.


*/
  