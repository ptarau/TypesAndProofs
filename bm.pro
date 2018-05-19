bm(N,P):-
  bmark(N,P,Res),
  writeln(Res).
 
cbm(N,P):-
  bmark(N,P,Res),
  writeln(Res).

bmark(N,P,Res):-prep(P,BL),!,
 bmark(N,P,nfTypes,allImpFormulas,BL,Res).

cbmark(N,P,Res):-prep(P,BL),!,
 bmark(N,P,nfTypes,allClassFormulas,BL,Res).

rbmark(N,P,Res):-prep(P,BL),!,
  bmark(N,P,ranSeededTNF,ranImpSeededFormulas,BL,Res).

ranSeededTNF(N,T):-
  Seed=42,K=100,
  ranSeededTNF(Seed,N,K,T),
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
preprocessor(pprove,(=)).
preprocessor(sprove,(=)).
preprocessor(hprove,toHorn).
preprocessor(timed_hprove,toHorn).
preprocessor(timed_hprove(_),toHorn).
preprocessor(xprove,toHorn).
preprocessor(hhprove,toSortedHorn).
preprocessor(vprove,toListHorn).
preprocessor(fprove,toListHorn).
preprocessor(gprove,dneg_expand).
preprocessor(kprove,expand_neg).
preprocessor(tprove,(=)).
preprocessor(tautology,false2neg).
preprocessor(badProve,(=)).
preprocessor(looper,(=)).
preprocessor(rprove,(toRandomHorn)).


nice_num(X,R):-R is (truncate(X*1000))/1000.



hbm(N,P,Counts,Time=T2-T1):-
  new_ctr(All),
  time(
  do((
   allSortedHorn(N,T),
   ctr_inc(All)
  )),
  T1
  ),
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
  Counts=Pr/Tot.
  
