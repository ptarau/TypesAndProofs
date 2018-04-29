% Generates all trees of with N internal nodes and
% it  collects their leaves to a list of logic variables

genTree(N,Tree,Leaves):-genTree(Tree,N,0,Leaves,[]).

genTree(V,N,N)-->[V].
genTree((A->B),SN1,N3)-->{SN1>0,N1 is SN1-1},
  genTree(A,N1,N2),
  genTree(B,N2,N3).
  
