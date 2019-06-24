% generic counting mechanisms for generator F, sizes up to M

countGen3(F,M,Rs):-
  findall(R,(
    between(1,M,N),
    sols(call(F,N,_,_),R)
  ),Rs).  

countGen2(F,M,Rs):-
  findall(R,(
    between(1,M,N),
    sols(call(F,N,_),R)
  ),Rs). 
   
  
% A000108: Catalan numbers [1,2,5,14,42,132,429,1430,4862,16796]
countHornTrees(M,Rs):-countGen3(genHorn,M,Rs).

% A004111		Number of rooted identity trees
% 1,1,2,3,6,12,25,52,113,247,548,1226
countSetTrees(M,Rs):-countGen2(genSetTree,M,Rs).

% A105633: [1,2,4,9,22,57,154,429,1223,3550,10455,31160,93802,284789]
countSortedHorn(M,Rs):-countGen3(genSortedHorn,M,Rs).

% A105633: 1,2,4,9,22,57,154,429,1223,3550,10455,31160
countStrictHorn(M,Rs):-countGen3(genStrictHorn,M,Rs).




% 1,2,4,8,20,47,122,316,845,2284,6264,17337,48424,136196,385548  
countSortedHorn3(M,Rs):-countGen3(genSortedHorn3,M,Rs).

% [1,1,2,5,13,37,109,331,1027,3241,10367,33531,109463] this
% [1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012] vs Catalans
countHorn3(M,Rs):-countGen2(genHorn3,M,Rs).
    
countTestSortedHorn(M,Ks):-countGen2(testSortedHorn,M,Ks).

% 1, 4, 21, 145, 1208, 11664, 127019
countAllStrictHorn(M,Rs):-countGen2(allStrictHorn,M,Rs).

bellHornTest(M,Ks):-
  findall(K,(
      between(1,M,I),succ(I,SI),
      sols(testSortedHorn(I,_),K1),
      bell(SI,K2),
      K is K1*K2
    ),
    Ks
  ).
  
% A105633 1,2,4,9,22,57,154,429,1223,3550,10455,31160
countSortedHornTrees(M):-countGen3(genSortedHorn,M,R),ppp(R).
  
  
% 2, 7, 38, 266, 2263, 22300, 247737  
countSortedHorn(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allSortedHorn,hprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).

countHorn(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allHornFormulas,hprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs). 
  
countSortedHorn3(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allSortedHorn3,hprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).

countAllHorn(M):-ncounts(M,allHornFormulas(_,_)).
  
countAllSortedHorn(M,Ks):-countGen2(allSortedHorn,M,Ks).

countAllSortedHorn(M):-ncounts(M,allSortedHorn(_,_)).

countAllSortedHorn3(M):-ncounts(M,allSortedHorn3(_,_)).


countFull(M,Rs):-countGen3(genOpTree,M,Rs).

countEqNeg(M,Rs):-countGen2(allEqNegFormulas,M,Rs).
  
countFullSorted(M,Rs):-countGen3(genSortedTree,M,Rs).

    
countFullTrimmed(M,Rs):-countGen3(genTrimmedTree,M,Rs).

countAllFull(M):-
  findall(N-R,(
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
  
countAllTrimmedFull(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allTrimmedFormulas,faprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).  
  
  
countAllSortedFullDyckhoff(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allSortedFullFormulas,dprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).  
  
/*

?- countAllFull(9).
 same as
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


countAllNegImp(M,P):-
  findall(R,(
      N to M,
      gen_and_count(N,allNegImpFormulas,P,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).  
  
countAllHarrop(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allHarropFormulas,eprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs). 

countAllSortedFull(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allSortedFullFormulas,faprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).  
  
:-dynamic(yes/1).

gen_and_count(N,G,P,[proven=Proven,total=Total,ratio=Ratio]):-
  retractall(proven(_)),
  init(total_count),
  init(proven_count),
  do((
    call(G,N,T),
    inc(total_count),
    call(P,T),
    inc(proven_count),
    form2tuple(T,TT),
    assertz(yes(TT))
  )),
  total(total_count,Total),
  total(proven_count,Proven),
  R is Proven/Total,
  %save_proven,
  nice_num(R,Ratio).
  

save_proven:-
  findall(X,yes(X),Xs),
  tell('yes.py'),
  write('yes = '),write_canonical(Xs),nl,
  told.

countAllImp(M):-
  findall(R,(
      N to M,
      gen_and_count(N,allImpFormulas,hprove,R)
    ),
    Rs
  ),
  maplist(ppp,Rs).  
  
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

  
nthImpFormula(N,T,K):-
  new_ctr(C),
  allImpFormulas(N,T),
  ctr_get(C,K),
  ctr_inc(C).
  
  
  