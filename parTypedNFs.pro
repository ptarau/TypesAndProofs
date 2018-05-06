% parallel version of the same
% if seen a number instead of the atom 'random'
% the generation is replicable/deterministic

parRanTypedTNF(Seed,TSize,N,K,X:T,Size):-
  set_random(seed(Seed)),
  MaxSteps=1000000,
  Max is truncate(N*(11/10)),
  Min is truncate(N*(9/10)),  
  between(1,K,_),
  parRanTypableNF(Max,Min,TSize,MaxSteps,X,T,Size,_Steps).
  
  
parRanTypableNF(Max,Min,TSize,MaxSteps,X,T,Size,Steps):-
  G=tryRanTypableNF(Max,Min,TSize,MaxSteps,X,T,Size,Steps),
  thread_count(L),
  ranseeds(L,Xs),
  length(Gs,L),
  maplist(add_seed(G),Xs,Gs),
  first_solution(G,Gs,[on_fail(continue)]).
  

add_seed(G,Seed,(set_random(seed(Seed)),G)). 

ranseeds(L,Xs):-
  findall(X,
      (between(1,L,_),X is random(2^32)),
  Xs).

  