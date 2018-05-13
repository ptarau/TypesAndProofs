bm(N,P):-
  bmark(N,P,Res),
  writeln(Res).
 
cbm(N,P):-
  bmark(N,P,Res),
  writeln(Res).

  
bmark(N,P,Res):-prep(P,BL),!,bmark(N,P,nfTypes,allImpFormulas,BL,Res).

cbmark(N,P,Res):-prep(P,BL),!,bmark(N,P,nfTypes,allClassFormulas,BL,Res).

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
 
prep(dprove,(=)).
prep(lprove,(=)).
prep(bprove,(=)).
prep(pprove,(=)).
prep(hprove,toHorn).
prep(xprove,toHorn).
prep(hhprove,toSortedHorn).
prep(vprove,toListHorn).
prep(fprove,toListHorn).
prep(gprove,dneg_expand).
prep(kprove,expand_neg).
prep(tautology,false2neg).

do(Goal):-
  Goal,
  fail
; true.

nice_num(X,R):-R is (truncate(X*100))/100.