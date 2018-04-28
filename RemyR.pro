% random binary tree with N internal nodes 
% (built with ->/2) and variables in Vs as leaves

remy(N,Tree,Vs):-remyR(N,Tree,Vs).
  
remyR(N,Tree,Vs):-
  Size is 2*N+1,
  functor(Links,x,Size),
  set(Links,0,0),
  remyStep(0,N,Links),
  get(Links,0,Root),
  links2bin(Root,Links,Tree,Leaves,[]),
  Vs=Leaves.
  
links2bin(K,Links,Tree,Ts1,Ts3):-
  ( K mod 2 =:= 0 -> Ts1=[Tree|Ts3]
  ; get(Links,K,A),
    K1 is K+1,
    get(Links,K1,B),
    links2bin(A,Links,X,Ts1,Ts2),
    links2bin(B,Links,Y,Ts2,Ts3),
    Tree=(X->Y)
  ).

set(Links,I0,Tree):-I is I0+1,nb_setarg(I,Links,Tree).
get(Links,I0,Tree):-I is I0+1,arg(I,Links,Tree).

remyStep(N,N,_).
remyStep(I0,N,Links):-I0<N,
  X is random(4*I0+2),
  I is I0+1,
  B is X mod 2,
  K is X // 2,
  J is I*2,
  J1 is J-B,
  J2 is J-1+B,
  set(Links,J1,J),
  get(Links,K,Y),set(Links,J2,Y),
  J3 is J-1,
  set(Links,K,J3),
  remyStep(I,N,Links).
  

  