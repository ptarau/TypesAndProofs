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

% set aprtitions via variable equlaities for length N
vpartitions(N,Ps):-length(Ps,N),mpart_of(Ps,_).

% just the partitions as equalities
vpartitions(Vs):-mpart_of(Vs,_).
