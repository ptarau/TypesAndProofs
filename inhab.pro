
% IHABITABILITY  
  
% tree with no inhabitant 
unInhabitableImplTree(N,T):-
  genTree(N,T,Vs),
  uninhabitable(T,Vs),
  maplist(=(0),Vs).  

  
uninhabitables(To,K,T):-uninhabitables(0,To,K,T).

uninhabitables(From,To,K,T):-
  between(From,To,K),
  t_(K,T,Vs),
  uninhabitable(T,Vs),
  maplist(=(0),Vs).
  

uninhabitable(T,Vs):-
  \+ (
    natpartitions(Vs),
    eprove(T)
  ).
  
  
% same but with nested Horn clauses
unInhabitableTree(N,T):-
  genSortedHorn(N,T,Vs),
  \+ (
    natpartitions(Vs),
    hprove(T)
  ).

  
% partition of variables, such that no tree
% labeled with them is inhabitable
unInhabitableVars(N,Vs):-N>0,
  N1 is N-1,
  vpartitions(N,Vs),natvars(Vs),
  \+ (
    genSortedHorn(N1,T,Vs),
    hprove(T)
    ).  
