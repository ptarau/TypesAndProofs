c:-['tp.pro']. % quick iteractive reloader

:-include('stats.pro'). % tools, including statistical and displayers

:-include('allTrees.pro').
:-include('remyR.pro').  % random binary tree, Knuth's algorithm R
%:-include('remyP.pro'). % declarative alternative

:-include('allPartitions.pro'). % partitions of set of given size
:-include('ranPartition.pro'). % random partitions

:-include('betaReducer.pro'). % reducers for lambda terms in de Bruijn from

:-include('allTypedNFs.pro'). % generator for simply typed normal forms
:-include('ranNormalForms.pro'). % random normal form generator (Boltzmann)

:-include('classTaut.pro'). % basic classical tautology testers

:-include('iProvers.pro'). % provers of implicational intuitionistic logic

:-include('hProvers.pro'). % other provers (also with embedded Horn clauses)


:-include('toHorn.pro'). % translators to/from embeded Horn Clauses

:-include('parProgs.pro'). % parallel variants of some of the programs

:-include('tester.pro'). % soup of testing and benchmarcking programs

:-include('third_party/dyckhoff.pro'). % implicational variant of Roy Dyckhoff's prover  
:-include('third_party/fitting.pro'). % implicational variant of M. Fitting's prover


% API elements

% all implicational logic formulas of size N
allImpFormulas(N,T):-
  genTree(N,T,Vs),
  vpartitions(Vs),
  natvars(Vs).
 
allClassFormulas(N,T):-
  genTree(N,T,Vs),
  vpartitions(Vs),
  classvars(Vs).
  
% all Glivenko classic formulas  
allClassFormulas(N,T,NNT):-
  genTree(N,T,Vs),
  vpartitions(Vs),
  dneg(T,NNT),
  natvars(Vs).
 
dneg(X,((X->false)->false)).
 
% random implicational logic formulas

ranImpFormula(N,T):-ranImpFormula(random,N,T).

ranImpFormula(Seed,N,T):-
  set_random(seed(Seed)),
  N1 is N+1,
  ranSetPart(N1,Vs),
  remy(N,T,Vs).

ranImpFormulas(N,K,T):-ranImpFormulas(random,N,K,T).

ranImpFormulas(Seed,N,K,T):-
  Count is round(sqrt(K)),
  ranImpFormulas(Seed,N,Count,Count,T).
  
ranImpFormulas(Seed,N,PartCount,TreeCount,T):-
  set_random(seed(Seed)),
  N1 is N+1,
  bell(N1,Bell),
  between(1,TreeCount,_),
  remy(N,T,Vs),
  between(1,PartCount,_),
  ranPart(N1,Bell,Vs).
  

  
  