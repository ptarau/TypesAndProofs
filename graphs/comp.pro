comps(Count):-
  graph(Xs),
  sort(Xs,Cs),
  length(Cs,Count).

graph([N0,N1,N2,N3,N4,N5,N6]):-
  edge(N0,N1),
  edge(N1,N3),
  edge(N3,N2),
  edge(N2,N0),
  edge(N2,N1),
  edge(N4,N5),
  edge(N5,N6),
  edge(N6,N4).

edge(X,X).



/*

*/
