% random partition generator

%:-use_module(library(tabling)).
:-table(stir/3).

ranSetPart(N,Rs):-bell(N,B),ranPart(N,B,Rs).

ranSetParts(K,N,Rs):-
  bell(N,B),
  between(1,K,_),
  ranPart(N,B,Rs).
 
% generates encoding of random set partition 
% given bell number B
ranPart(N,B,Rs):-
  pickUrns(N,B,M),
  functor(Norm,x,M),
  functor(Bs,x,N),
  ranLoop(Bs,Norm,M,0,N,0,_),
  Bs=..[_|Rs].

  
% stirling numbers of 2nd kind
stir(0,0,1).
stir(N,0,0):-N>0.
stir(0,N,0):-N>0.
stir(N1,K,S):-N1>0,K>0,N is N1-1,K1 is K-1,
 stir(N,K,S1),
 stir(N,K1,S2),
 S is K*S1+S2.
 
% bell numbers 
bell(N,B):-
  numlist(0,N,Ks),
  maplist(stir(N),Ks,Stirs),
  sum_list(Stirs,B).

% probabilty for urn M
urn_probability(N,M,B,R):-
  MN is M^N,
  factorial(M,F),
  R is MN rdiv F rdiv B rdiv rational(e).
  
factorial(0,1).
factorial(N,R):-N>0,N1 is N-1,factorial(N1,R1),R is N*R1.

% picks number of available urns
pickUrns(N,B,U):-
  random(P),
  pickUrns(P,N,B,0, U).

% picks number of available urns, for prob=P  
pickUrns(P,_N,_B,U,U):-P=<0.
pickUrns(P1,N,B,U1,U3):-P1>0,
  U2 is U1+1,
  urn_probability(N,U2,B,R),
  P2 is P1-R,
  pickUrns(P2,N,B,U2,U3).

% arg/3, from 0 to n-1  
arg0(I,T,X):-succ(I,J),arg(J,T,X).


ranLoop(_Bs,_Norm,_,I,N,C,C):-I>=N.
ranLoop(Bs,Norm,M,I,N,C1,C3):-I<N,
    K is random(M),
    normalize(Norm,K,C1,C2),
    arg0(K,Norm,C),
    arg0(I,Bs,C),
    J is I+1,
    ranLoop(Bs,Norm,M,J,N,C2,C3).
    
normalize(Norm,K,C1,C2):-
   arg0(K,Norm,V),
   var(V),
   !,
   V is C1,
   C2 is C1+1.
normalize(_,_,C,C).
  

/*
% ?- stir(50,13,R).
% R = 6238901276275784811492861794826737563889288230.

?- bell(10,B),urn_probability(10,5,B,U).
B = 115975,
U = 0.25814275114535223 

?- N=3,bell(N,B),ranSet(N,B,R).
N = 3,
B = 5,
R = [0, 1, 2] ;

*/
