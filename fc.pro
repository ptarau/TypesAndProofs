:-include('/Users/tarau/Desktop/sit/SOFTWARE/PROVERS/fCube-4.1/fCube/fCube').

fcube(A):-toPrefix(A,X),intDecide0(X,_).

fc(X):-intDecide0(X,_).


intDecide0(X,COUNTERMODEL):-	
  permanenzaSegno([swff(f,X)],StartingSet),
  orderEquivSet(StartingSet, OrderedStartingSet),
  reapply(OrderedStartingSet, COUNTERMODEL, 1, 1),!,
  fail.
intDecide0(_,[valida]).

fcok:-mints((p <-> ~q)<-> (~q <-> p),T),ppp(T),faprove(T).

fcbug:-mints((p <-> ~q) <-> (~q <-> p),T),ppp(T),fcube(T).