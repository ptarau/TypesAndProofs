% natural number -> hereditatily finite set
% with urelments smaller than M and
% a constraint that make them look like
% nested Horn terms

nat2hhfs(M,U,U):-U<M.
nat2hhfs(M,N,(H:-Hs)):-N>=M,
  nat2unats(M,N,H-Ns),
  maplist(nat2hhfs(M),Ns,Hs).

nat2unats(M,N,D-Ns):-
  get_bdigit(M,D,N,NewN),
  nat2nats(NewN,Ns).

unats2nat(M,D-Ns,N):-
  nats2nat(Ns,OldN),
  put_bdigit(M,D,OldN,N).

hhfs2nat(M,H,H):-atomic(H),assertion(M>H).
hhfs2nat(M,(H:-Bs),N):-assertion(M>H),
  maplist(hhfs2nat(M),Bs,Cs),
  unats2nat(M,H-Cs,N).

% extracts a bijective base B digit D from N leaving NewN
/*
get_bdigit etracts a bijective base B digit
D from a positive natural number N and returns
  a (possibly 0) remainder NewN.

*/
get_bdigit(B,D,N,NewN):-N>0,
  Q is N // B,
  D0 is N mod B,
  ( D0 =:= 0 ->  D is B-1, NewN is Q-1
  ; D is D0-1, NewN is Q
  ).

% adds a bijective base B digit D to N giving NewN
/*
put_bdigit adds a binary base B digit D to a natural
number N resulting in a strictly positive natural
number NewN. One can see this operation as adding
exactly the information corresponding to a choice
between B possibilities to N. This property
ensures that our invented numbering system
works in a way similar to factoradics.
*/
put_bdigit(B,D,N,NewN):- D >= 0,D<B,NewN is 1+D+B*N.



test_hhfs:-
   U=5,
   M=1000,
   between(0,M,N),
   nat2hhfs(U,N,HBs),
   hprove(HBs),
   hhfs2nat(U,HBs,NN),
   ppp((N=NN):'------>':HBs),
   pph(HBs),
   fail.
