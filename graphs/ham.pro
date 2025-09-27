graph([N0,N1,N2,N3]):-
  edge(N0,N1),
  edge(N1,N3),
  edge(N3,N2),
  edge(N2,N0),
  edge(N2,N1).

edge(X,Y):-nonvar(X),nonvar(Y),!.
edge(X,Y):-step(X,Y).

step(0,1).
step(1,2).
step(2,3).
step(3,0).



/*
e.g. hamiltonian
todo: some edges form the same
      node are ok to be left out
*/
