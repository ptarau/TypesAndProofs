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
  findall(R,(
    between(1,M,N),
    sols(genHorn(N,_,_),R)
  ),Rs).
  
/*  
% [1,2,7,38,266,2263,22300,247737]
genSortedHorn(N,Tree,Leaves):-
  genSortedHorn(Tree,N,0,Leaves,[]).

genSortedHorn(V,N,N)-->[V].
genSortedHorn((A:-[B|Bs]),SN1,N3)-->{succ(N1,SN1)},
  [A],
  genSortedHorn(B,N1,N2),
  genSortedHorns(Bs,N2,N3),
  {sorted([B|Bs])}.
  
genSortedHorns([],N,N)-->[].
genSortedHorns([B|Bs],SN1,N3)-->{succ(N1,SN1)},
  genSortedHorn(B,N1,N2),
  genSortedHorns(Bs,N2,N3).
  
sorted([]):-!.
sorted([_]):-!.
sorted([X,Y|Xs]):-X@<Y,sorted([Y|Xs]).  
*/

% A105633: [1,2,4,9,22,57,154,429,1223,3550,10455,31160,93802,284789]
genSortedHorn(N,Tree,Leaves):-
  genSortedHorn(Tree,N,0,Leaves,[]).

genSortedHorn(V,N,N)-->[V].
genSortedHorn((A:-[B|Bs]),SN1,N3)-->{succ(N1,SN1)},
  [A],
  genSortedHorn(B,N1,N2),
  genSortedHorns(B,Bs,N2,N3).
  
genSortedHorns(_,[],N,N)-->[].
genSortedHorns(B,[C|Bs],SN1,N3)-->{succ(N1,SN1)},
  genSortedHorn(C,N1,N2),
  {B@<C},
  genSortedHorns(C,Bs,N2,N3).
  
countSortedHorn(M,Rs):-
  findall(R,(
    between(1,M,N),
    sols(genSortedHorn(N,_,_),R)
  ),Rs).
  
  
genSortedHorn3(N,Tree,Leaves):-
  genSortedHorn3(3,Tree,N,0,Leaves,[]).

genSortedHorn3(_,V,N,N)-->[V].
genSortedHorn3(SK,(A:-[B|Bs]),SN1,N3)-->{succ(N1,SN1),succ(K,SK)},
  [A],
  genSortedHorn3(K,B,N1,N2),
  genSortedHorn3s(SK,B,Bs,N2,N3).
  
genSortedHorn3s(_,_,[],N,N)-->[].
genSortedHorn3s(K,B,[C|Bs],SN1,N3)-->{succ(N1,SN1)},
  genSortedHorn3(K,C,N1,N2),
  {B@<C},
  genSortedHorn3s(K,C,Bs,N2,N3).

% 1,2,4,8,20,47,122,316,845,2284,6264,17337,48424,136196,385548  
countSortedHorn3(M,Rs):-
  findall(R,(
    between(1,M,N),
    sols(genSortedHorn3(N,_,_),R)
    ),Rs).  
    
    
