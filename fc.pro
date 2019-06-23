:-include('../SOFTWARE/PROVERS/fCube-4.1/fCube/fCube').

fcube(A):-toPrefix(A,X),intDecide0(X,_).

fc(X):-intDecide0(X,_).


intDecide0(X,COUNTERMODEL):-	
  permanenzaSegno([swff(f,X)],StartingSet),
  orderEquivSet(StartingSet, OrderedStartingSet),
  reapply(OrderedStartingSet, COUNTERMODEL, 1, 1),!,
  fail.
intDecide0(_,[valida]).

notfc:-T0=(p <-> ~q)<-> (~q <-> p),ppp(T),
  mints(T0,T),ppp(T),faprove(T),dprove(T).

fcbug:-T0=(p <-> ~q)<-> (~q <-> p),
   fcube(T0),ppp(should_be_true(T0)),
   mints(T0,T),
   ppp(trying_equivalent=T),
   fcube(T).
   
fcbug1:-
  T0=((p <-> ~q)-> (~q -> p)),
   fcube(T0),ppp(should_be_true(T0)),
   mints(T0,T),
   ppp(trying_equivalent=T),
   fcube(T).
  
fcbug2:-
  T0=((~p <-> q)-> (q -> ~p)),
  mints(T0,T),
  fcube(T0),ppp(should_be_true(T0)),
  mints(T0,T),
  ppp(trying_equivalent=T),
  fcube(T).