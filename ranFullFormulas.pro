
ranFullFormula(N,T):-ranFullFormula(random,N,T).

ranFullFormula(Seed,N,T):-ranFullFormula(Seed,N,[(->),(&),(v),(<->)],T).

/*
ranFullFormula(Seed,N,FunList,T):-
  set_random(seed(Seed)),
  succ(N,SN),ranSetPart(SN,Vs),
  sort(Vs,Sorted),length(Sorted,L),
  K is random(L),
  replace(K,false,Vs,NewVs),
  remyExpr(N,FunList,T,NewVs).
  
replace(_,_,[],[]):-!.
replace(X,Y,[X|Xs],[Y|Ys]):-!,replace(X,Y,Xs,Ys).
replace(X,Y,[A|Xs],[A|Ys]):-replace(X,Y,Xs,Ys).    
*/

ranFullFormula(Seed,N,FunList,T):-
  set_random(seed(Seed)),
  ranFullPosFormula(N,FunList,P),
  negDec(P,T).

ranFullPosFormula(N,FunList,T):-
  succ(N,SN),ranSetPart(SN,Vs),
  remyExpr(N,FunList,T,Vs).
  
  
negDec((X->Y),D):-!,negDec(X,A),negDec(Y,B),decOne((A->B),D).
negDec((X<->Y),D):-!,negDec(X,A),negDec(Y,B),decOne((A<->B),D).
negDec((X & Y),D):-!,negDec(X,A),negDec(Y,B),decOne((A & B),D).
negDec((X v Y),D):-!,negDec(X,A),negDec(Y,B),decOne((A v B),D).
negDec(X,DX):-decOne(X,DX).

decOne(X,DX):-Choice is random(3),decOne(Choice,X,DX).

decOne(0,X,X).
decOne(1,X,~X).
decOne(2,X,~ ~ X).


ranFlatFormula(Seed,N,MT):-
  ranFullFormula(Seed,N,T),
  mints(T,H,Bs),
  unexpand(Bs,H,MT).