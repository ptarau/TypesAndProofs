% random implicational logic formulas
% relies on ranPartition and RemyR (random binary trees)

ranImpFormula(N,T):-ranImpFormula(random,N,T).

ranImpFormula(Seed,N,T):-
  set_random(seed(Seed)),
  N1 is N+1,
  ranSetPart(N1,Vs),
  remy(N,T,Vs).

ranImpFormulas(N,K,T):-ranImpFormulas(random,N,K,T).

% variant neede for gold standard tester
genRanImpFormulas(K,N,T):-ranImpFormulas(N,K,T).

ranImpFormulas(Seed,N,K,T):-
  Count is round(sqrt(K)),
  ranImpFormulas(Seed,N,Count,Count,T).
  
ranImpFormulas(Seed,N,PartCount,TreeCount,T):-
  set_random(seed(Seed)),
  N1 is N+1,
  bell(N1,Bell),
  between(1,TreeCount,_),
  remy(N,T,Vs),
  between(1,PartCount,_),
  ranPart(N1,Bell,Vs).
 
ranTypedImp(MaxSeed,M,N,T):-
  between(1,MaxSeed,Seed),
  ranImpFormula(Seed,M,T),
  hprove(T),
  sprove(T,X),
  type_of(X,TT),
  tsize(TT,Size),
  Size>N.
  
bigTypedImp(T):-ranTypedImp(20000,36,16,T).  

bigtest:-do((bigTypedImp(T),hprove(T),ppp(T))).


ranShow:-MSeed=1000,M=7,
 do((
   between(0,MSeed,Seed),
   ranImpFormula(Seed,M,T0),
   sprove1(T0,X),lsize(X,S),S>=M,
   varvars(T0,T),
   ppp(T),
   ppp(X),
   nl,
   ppt(X:T),
   qqq(X:T),
   nl,
   ppp('-------------')
 )).
  

/*

Can we generate a formula (that has a proof!) that is so hard that we cannot prove it?

*/