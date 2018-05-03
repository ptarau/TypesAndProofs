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
