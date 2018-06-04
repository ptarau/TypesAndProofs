% partitions with variables replaced with natural numbers
natpartitions(Vs):-
   mpart_of(Vs,Ns),
   length(Ns,SL),
   succ(L,SL),
   numlist(0,L,Ns).

% computes set partitions seen as distinct logic variables
% second arg has the unique variables

mpart_of([],[]).
mpart_of([U|Xs],[U|Us]):-
  mcomplement_of(U,Xs,Rs),
  mpart_of(Rs,Us).

% mimic computing the complement
% but just fuse logic variables
% representing equivalence classes

mcomplement_of(_,[],[]).
mcomplement_of(U,[X|Xs],NewZs):-
  mcomplement_of(U,Xs,Zs),
  mplace_element(U,X,Zs,NewZs).

mplace_element(U,U,Zs,Zs).
mplace_element(_,X,Zs,[X|Zs]).

% just the partitions as equalities
vpartitions(Vs):-mpart_of(Vs,_).



% set partitions via variable equalities for length N
% counted by Bell numbers: A000110
% 1, 1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975, 678570,...
vpartitions(N,Ps):-length(Ps,N),mpart_of(Ps,_).


/*
?- findall(S,(between(0,10,N),sols(vpartitions(N,_),S)),Xs),ppp(Xs).
Xs = [1,1,2,5,15,52,203,877,4140,21147,115975].
*/