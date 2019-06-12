% transformations turning formulas into equivalent, herd to prove ones

allFullHardenedFormulas(N,HT):-
  allTrimmedFormulas(N,T),
  mints(T,HT).
  
allFullDBFormulas(N,HT):-
  allTrimmedFormulas(N,T),
  toDisjBiCond(T,HT).
  
