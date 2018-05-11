% input transformers  
  
% turns a term in which variables are repsentede as Prolog vars  
varvars(A,X):-
  maxvar(A,L0),L is L0+1,
  functor(D,x,L),
  varvars(A,X,D).

varvars((A,B),(X->Y),D):-varvars(A,X,D),varvars(B,Y,D).
varvars(A->B,X->Y,D):-varvars(A,X,D),varvars(B,Y,D).
varvars(A,V,D):-integer(A),I is A+1,arg(I,D,V).
varvars(false,false,_).

% variable with larges index
maxvar(I,R):-integer(I),!,R=I.
maxvar(false,0):-!.
maxvar((A->B),R):-maxvar(A,I),maxvar(B,J),R is max(I,J).
maxvar((A,B),R):-maxvar(A,I),maxvar(B,J),R is max(I,J).

% turns a term into a ground one by banding
% logic variables in it to 0,1,...

natvars(T):-
  must_be(acyclic,T),
  term_variables(T,Vs),
  length(Vs,L1),
  L is L1-1,
  numlist(0,L,Vs).

% same, but throws in atom "false"
% as first variable to bind
classvars(T):-
  must_be(acyclic,T),
  term_variables(T,[false|Vs]),
  length(Vs,L1),
  L is L1-1,
  numlist(0,L,Vs).  
  
imp2eqs(Imp,R-Es):-imp2eqs(Imp,R,Es,[]).

imp2eqs(A->B,R)-->!,
  imp2eqs(A,X),
  imp2eqs(B,Y),
  [R=X-Y].
imp2eqs(A,A)-->[].
