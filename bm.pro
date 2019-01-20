ibm:-ibm(7,bprove).
  
ibm(M,P):-M1 is M-1,
 do((
   between(0,M1,N),
   time(seqCountProvenFormulas(N,P,A,B),T),
   writeln([n=N,prover=P,proven=A,total=B,time=T])
 )).

bm(N,P):-
  bmark(N,P,Res),
  writeln(Res).
 
cbm(N,P):-
  cbmark(N,P,Res),
  writeln(Res).

bmark(N,P,Res):-prep(P,BL),!,
 bmark(N,P,nfTypes,allImpFormulas,BL,Res).

cbmark(N,P,Res):-prep(P,BL),!,
 bmark(N,P,nfTypes,allClassFormulas,BL,Res).

rbmark(N,P,Res):-prep(P,BL),!,
  bmark(N,P,ranTNF,ranImpSeededFormulas,BL,Res).

ranSeededTNF(N,T):-
  Seed=42,K=100,
  ranTNF(Seed,N,K,T,_Sizes),
  %ppp(T),
  true.

ranImpSeededFormulas(N,T):-
  Seed=42,K=100,
  ranImpFormulas(Seed,N,K,T),
  %ppp(T),
  true.
  
fbmark(MaxTime,P,Res):-
  consult('test_data/proven.pro'),
  prep(P,BL),
  !,
  bmark(0,maxTimed(MaxTime,P),fpos,fneg,BL,Res).
  
maxTimed(MaxTime,P,T):-
  %ppp(T),
  timed_call(MaxTime,call(P,T),R),
  (number(R)->true;ppp(T),ppp(R),nl).
  
fpos(_,T):-
  proven(true,T),
  %ppp(T),
  true.
  
fneg(_,T):-
  proven(false,T),
  %ppp(T),
  true.
  
bmark(N,P,Pos,Neg,BL,Res):-
  N2 is N // 2,
  time(
  do((
    call(Pos,N,T0),
    call(BL,T0,_)
  )),
  TP0
  ),
  time(
  do((
    call(Neg,N2,T0),
    call(BL,T0,_)
  )),
  TN0
  ),
  time(
  do((
    call(Pos,N,T),
    call(P,T)
  )),
  TP1
  ),
  time(
  do((
    call(Neg,N2,T),
    call(P,T)
  )),
  TN1
  ),
  TP is TP1-TP0,
  TN is TN1-TN0,
  Tot is TP+TN,
  maplist(nice_num,[TP,TN,Tot],[XTP,XTN,XTot]),
  Res=[prog=P,size=N,pos=XTP,neg=XTN,total=XTot].
 
prep(Name,Proc):-preprocessor(Name,Proc),!.
prep(Name,Prog):-throw(unknown_prep(Name,Prog)).

preprocessor(dprove,(=)).
preprocessor(lprove,(=)).
preprocessor(bprove,(=)).
preprocessor(eprove,(=)).
preprocessor(efprove,(=)).
preprocessor(nprove,(=)).
preprocessor(pprove,(=)).
preprocessor(qprove,(=)).
preprocessor(sprove,(=)).
preprocessor(faprove,(=)).
preprocessor(fxprove,(=)).
preprocessor(alt_prove,(=)).
preprocessor(ffprove,(=)).
preprocessor(fcprove,(=)).
preprocessor(fdprove,(=)).
preprocessor(hprove,toHorn).
preprocessor(hrprove,toHorn).
preprocessor(hgprove,toHorn).
preprocessor(h1prove,toHorn).
preprocessor(h3prove,toHorn).
preprocessor(hhprove,toHorn).
preprocessor(hhhprove,toHorn).
preprocessor(hh1prove,toHorn).
preprocessor(nhprove,toAHorn).
preprocessor(ahprove,toAHorn).
preprocessor(hvprove,toVarHorn).
preprocessor(wprove,toFlatHorn).
preprocessor(w3prove,toFlatHorn).
preprocessor(oprove,toHorn).
preprocessor(jprove,toHorn).
preprocessor(timed_hprove,toHorn).
preprocessor(timed_hprove(_),toHorn).
preprocessor(iprove,toHorn).
preprocessor(xprove,toHorn).
preprocessor(vprove,toListHorn).
preprocessor(fprove,toListHorn).
preprocessor(gprove,dneg_expand).
preprocessor(kprove,expand_neg).
preprocessor(tprove,(=)).
preprocessor(tautology,false2neg).
preprocessor(badProve,(=)).
preprocessor(looper,(=)).
preprocessor(rprove,(toRandomHorn)).
preprocessor(parProve,(=)).
preprocessor(parProveHorn,toHorn).


nice_num(X,R):-R is (truncate(X*1000))/1000.



hbm(N,P,Counts,Time=T2-T1):-
  assertion(member(P,
     [hrlj,ljh,hlj1,hhlj1,jlj,ljj,lji,ljy,ljz,ljg])),
  new_ctr(All),
  time(
  do((
   allSortedHorn(N,T),
   ctr_inc(All)
  )),
  T1
  ),
   ppp(t1=T1),
  new_ctr(Proven),
  time(
  do((
   allSortedHorn(N,T),
   call(P,T),
   ctr_inc(Proven)
  )),
  T2
  ),
  Time is T2-T1,
  ctr_get(Proven,Pr),
  ctr_get(All,Tot),
  R is Pr/Tot,
  Counts=(Pr/Tot=R).
  
fbm(N,P):-
  fbm(N,P,Counts,Time),
  ppp(counts=Counts),
  ppp(time=Time).
 
good_full_prover(P):-
   assertion(memberchk(P,
     [dprove,faprove,fxprove,flprove,frprove,ffprove,fcprove,fdprove,alt_prove])).

fbm(N,P,Counts,Time=T2-T1):-
  good_full_prover(P),
  new_ctr(All),
  time(
  do((
   allFullFormulas(N,T),
   ctr_inc(All)
  )),
  T1
  ),
   %ppp(t1=T1),
  new_ctr(Proven),
  time(
  do((
   allFullFormulas(N,T),
   %ppp(t=T),
   call(P,T),
   ctr_inc(Proven)
  )),
  T2
  ),
  Time is T2-T1,
  ctr_get(Proven,Pr),
  ctr_get(All,Tot),Ratio is Pr/Tot,
  Counts=(Pr/Tot=Ratio).

fsbm(N,P):-
  fsbm(N,P,Counts,Time),
  ppp(counts=Counts),
  ppp(time=Time).  
  
fsbm(N,P,Counts,Time=T2-T1):-
  good_full_prover(P),
  new_ctr(All),
  time(
  do((
   allSortedFullFormulas(N,T),
   ctr_inc(All)
  )),
  T1
  ),
   ppp(t1=T1),
  new_ctr(Proven),
  time(
  do((
   allSortedFullFormulas(N,T),
   call(P,T),
   ctr_inc(Proven)
  )),
  T2
  ),
  Time is T2-T1,
  ctr_get(Proven,Pr),
  ctr_get(All,Tot),R is Pr/Tot,
  Counts=(Pr/Tot=R).

  
/*
- fbm(7,ffprove,Counts,Time=T2-T1).
t1=0.2620220184326172
Counts = 3343/205674,
               Time = 1.3210039138793945,
T2 = 1.5830259323120117,
T1 = 0.2620220184326172.

?- fbm(7,faprove,Counts,Time=T2-T1).
t1=0.26184606552124023
Counts = 3343/205674,
               Time = 1.4115409851074219,
T2 = 1.673387050628662,
T1 = 0.26184606552124023.

?- fbm(7,dprove,Counts,Time=T2-T1).
t1=0.2626791000366211
Counts = 3343/205674,
              Time = 1.0432429313659668,
T2 = 1.305922031402588,
T1 = 0.2626791000366211.

?- fsbm(11,faprove,C,T).
t1=35.28143906593323
C = 1961346/27248400,
T =  (979.1363348960876=1014.4177739620209-35.28143906593323).

?- fsbm(11,dprove,C,T).
t1=35.10769701004028
C = 1961346/27248400,
T =  (226.97890901565552=262.0866060256958-35.10769701004028).

*/ 
  
itaut(N):-
  do((
  allFullFormulas(N,F),
  %simplify(F0,F),
  faprove(F),
  ppp(F)
  )).
  