graph([N0,N1,N2,N3]):-
  edge(N0,N1),
  %edge(N0,N2), % would need 4 colors
  edge(N0,N3),
  edge(N1,N2),
  edge(N1,N3),
  edge(N2,N3).

edge(X,Y):-dif_colors(X,Y).
edge(X,Y):-dif_colors(Y,X).

dif_colors(r,g).
dif_colors(g,b).
dif_colors(b,r).

/*
one can "compile" a graph coloring problem
into a Horn formula of size O(E)+O(C^2)

if X\=Y is part of the language,

dif_colors(X,Y) is simply: X\=Y
+
edge(X,Y):-color(X),color(Y),X\=Y.

with O(C) size

the logic variable representation supportes easy
graph contraction

It is NP-complete to decide if a given graph admits a k-coloring for a given k except for the cases k ∈ {0,1,2} . In particular, it is NP-hard to compute the chromatic number.

Quite inefficient:

generate all "interesting"
k-partitions of vars N0..Nm,
and for each test in O(m) that they do
not contains edges with equal ends.
A partition entails collapsing nodes
and "removing" e(X,X) edges
*/

/*
express connectedness?
connected components:

unify ends of each edge, count nb of distinct vars

implemented by adding edge(X,X) e.g. by asserting it
---

collapsing
kn edges - collapse edges with distance smaller than epsilon
after collapsing, collect vars from edge(X,X)

study what graph algorithms benefit from this representation
in terms of:
1. expressiveness
2. efficiency

possibly: add attributes to variables
representing node properties - e.g. original name of the node

e.g. hamiltonian - vars need dejavu attributes
*/
