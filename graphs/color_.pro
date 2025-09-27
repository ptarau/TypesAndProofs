graph([N0,N1,N2,N3]):-
  edge(N0,N1),
  %edge(N0,N2), % would need 4 colors
  edge(N0,N3),
  edge(N1,N2),
  edge(N1,N3),
  edge(N2,N3).

color(r).
color(g).
color(b).

edge(X,Y):-color(X),color(Y),X\=Y.

/*
?- graph(X).
X = [r, g, r, b] ;
X = [r, b, r, g] ;
X = [g, r, g, b] ;
X = [g, b, g, r] ;
X = [b, r, b, g] ;
X = [b, g, b, r] ;
false.
*/