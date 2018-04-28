% random binary tree with N internal nodes 
% (built with ->/2) and variables in Vs as leaves

remy(N,Tree,Vs):-remyP(N,Tree,Vs).

remyP(N,Tree,Vs):-
  remy_loop(N,Es,_),
  bind_nodes(Es,Tree,Leaves),
  Vs=Leaves.
  
  
remy_init([e(left,A,_),e(right,A,_)]).

left_or_right(I,J):-
  choice_of(2,Dice),
  left_or_right(Dice,I,J).

choice_of(N,K):-K is random(N).
% choice_of(N,K):-N>0,N1 is N-1,between(0,N1,K).

left_or_right(0,left,right).
left_or_right(1,right,left).

grow(e(LR,A,B), e(LR,A,C),e(I,C,_),e(J,C,B)):-
  left_or_right(I,J).

remy_step(Es,NewEs,L,NewL):-
  NewL is L+2,
  choice_of(L,Dice),
  remy_step1(Dice,Es,NewEs).

remy_step1(0,[U|Xs],[X,Y,Z|Xs]):-grow(U, X,Y,Z).
remy_step1(D,[A|Xs],[A|Ys]):-D>0,D1 is D-1,
  remy_step1(D1,Xs,Ys).

remy_loop(0,[],0).
remy_loop(1,Es,2) :-remy_init(Es).
remy_loop(K,NewEs,N3):-K>1, K1 is K-1,
  remy_loop(K1,Es,N2),
  remy_step(Es,NewEs,N2,N3).

bind_nodes([],_,[]).
bind_nodes([X|Xs],Root,Vs):-X=e(_,Root,_),
  maplist(bind_internal,[X|Xs]),
  term_variables(Root,Vs).
  
bind_internal(e(left,(A->_),A)).
bind_internal(e(right,(_->B),B)).


