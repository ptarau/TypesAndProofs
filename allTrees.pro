% Generates all trees of with N internal nodes and
% it  collects their leaves to a list of logic variables

genTree(N,Tree,Leaves):-genTree(Tree,N,0,Leaves,[]).

genTree(V,N,N)-->[V].
genTree((A->B),SN1,N3)-->{SN1>0,N1 is SN1-1},
  genTree(A,N1,N2),
  genTree(B,N2,N3).

% OEIS 1,2,5,14,42,132,429,1430,4862,16796 
genHorn(N,Tree,Leaves):-genHorn(Tree,N,0,Leaves,[]).

genHorn(V,N,N)-->[V].
genHorn((A:-[B|Bs]),SN1,N3)-->{succ(N1,SN1)},
  [A],
  genHorn(B,N1,N2),
  genHorns(Bs,N2,N3).
  
genHorns([],N,N)-->[].
genHorns([B|Bs],SN1,N3)-->{succ(N1,SN1)},
  genHorn(B,N1,N2),
  genHorns(Bs,N2,N3).

countGenHorn(M,Rs):-
  findall(R,(between(1,M,N),sols(genHorn(N,_,_),R)),Rs).
  
  