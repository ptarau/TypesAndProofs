% transformations turning formulas into equivalent, hard to prove ones

transform(N,Generator,Transformer,Prover,Generated,Formula):-
  call(Generator,N,Generated),
  call(Transformer,Generated,Formula),
  call(Prover,Formula).
  
% some instances  
  
hardTrue(N,Prover,Generated,Hard):-
  transform(N,implTaut,toDisjBiCond,Prover,Generated,Hard).

hardMints(N,Prover,Generated,Hard):-
  transform(N,allTrimmedFormulas,mints,Prover,Generated,Hard).

disjBiCondHard(N,Prover,Generated,Hard):-
  transform(N,allTrimmedFormulas,toDisjBiCond,Prover,Generated,Hard).
  
  
ht1:-do((hardTrue(6,dprove,T,R),ppp(T),ppp(R),nl)).

ht2:-do((hardTrue(6,fcube,T,R),ppp(T),ppp(R),nl)).

ht4:-do((hardTrue(4,faprove,T,R),ppp(T),ppp(R),nl)).
  
ht5:-do((hardMints(4,faprove,T,R),ppp(T),ppp(R),nl)).
    
  

  
