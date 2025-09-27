:-include('compile_dual.pro').

graph([N0,N1,N2,N3]) :-
  edge(N0,N1),
  %edge(N0,N2), % would need 4 colors
  edge(N0,N3),
  edge(N1,N2),
  edge(N1,N3),
  edge(N2,N3).

edge(X,Y) :- falsifiable:bad_edge(X,Y).

bad_edge(X,Y) -: same_color(X,Y).
bad_edge(X,Y) -: same_color(Y,X).

~same_color(r,g).
~same_color(g,b).
~same_color(r,b).
