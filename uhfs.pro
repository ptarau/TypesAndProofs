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

% natural number to nested Horn clause
nat2uhorn(M,U,U):-U<M.
nat2uhorn(M,N,HBs):-N>=M,
  nat2nats(N,Ns),
  maplist(nat2uhorn(M),Ns,Hs),
  hornify(Hs,HBs).

hornify([X],~X).
hornify([X,Y|Xs],(X:-[Y|Xs])).

% nested Horn clause -> natural number
uhorn2nat(U,U):-atomic(U).
uhorn2nat(HBs,N):-
  hornify(Hs,HBs),
  maplist(uhorn2nat,Hs,Ns),
  nats2nat(Ns,N).


test_uhorn:-
   U=5,
   M=1000,
   between(0,M,N),
   nat2uhorn(U,N,HBs),
   nhorn(HBs),
   uhorn2nat(HBs,NN),
   ppp((N=NN):'------>':HBs),
   pph(HBs),
   fail.
