% natural number -> hereditatily finite set
% with urelments smaller than M

nat2uhfs(M,U,U):-U<M.
nat2uhfs(M,N,Hs):-N>=M,
  nat2nats(N,Ns),
  maplist(nat2uhfs(M),Ns,Hs).

% hereditatily finite set -> natural number
uhfs2nat(U,U):-atomic(U).
uhfs2nat(Ns,N):-compound(Ns),
  maplist(uhfs2nat,Ns,Ms),
  nats2nat(Ms,N).

ensure_flat(H,[],HH):-atomic(H),!,HH=H.
ensure_flat(H,Bs,(HH:-Bs)):-atomic(H),!,HH=H.
ensure_flat((H:-Xs),Bs1,(H:-Bs2)):-append(Xs,Bs1,Bs2).

hornify(U,U):-atomic(U).
hornify([X],HBs):-hornify(X,H),ensure_flat(H,[],HBs).
hornify([X,Y|Xs],HBs):-
  hornify(X,H),
  maplist(hornify,[Y|Xs],Bs),
  ensure_flat(H,Bs,HBs).

nat2uhorn(M,N,C):-
  nat2uhfs(M,N,X),
  hornify(X,C).

/*
?- nat2uhorn(4,1001,R),ppp(R),pph(R),fail.
0:-[3,(0:-[2]),(1:-[2]),(0:-[1,2]),3,(0:-[3])]
          0
  ________|_______
 /  |  |   |    | \
 3  0  1   0    3  0
    |  |   |       |
    |  |  / \      |
    2  2  1  2     3

*/
