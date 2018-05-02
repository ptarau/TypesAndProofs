% all implicational logic formulas of size N
allImpFormulas(N,T):-
  genTree(N,T,Vs),
  vpartitions(Vs),
  natvars(Vs).
 
% all classical implicational formulas  
allClassFormulas(N,T):-
  genTree(N,T,Vs),
  vpartitions(Vs),
  classvars(Vs).
  
% all Glivenko tranformed classic formulas  
allClassFormulas(N,T,NNT):-
  genTree(N,T,Vs),
  vpartitions(Vs),
  dneg(T,NNT),
  natvars(Vs).
 
 
