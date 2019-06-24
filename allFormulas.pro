% all implicational logic formulas of size N
% ?- countGen2(allHornFormulas,6,R).
% R = [2, 10, 75, 728, 8526, 115764]
% A289679		a(n) = Catalan(n)*Bell(n+1).
allHornFormulas(N,T):-
  succ(N,SN),length(Vs,SN),
  natpartitions(Vs),
  genHorn(N,T,Vs).

% all Horn formulas with bodies in canonical order
% to break symmetries irrelevant for testing provers
allSortedHorn(N,T):-
  succ(N,SN),length(Vs,SN),
  natpartitions(Vs),
  genSortedHorn(N,T,Vs).


allStrictHorn(N,T):-
  succ(N,SN),length(Vs,SN),
  natpartitions(Vs),
  genStrictHorn(N,T,Vs).
  
testSortedHorn(N,T):-
  numlist(0,N,Vs),
  genSortedHorn(N,T,Vs).  
 
  
% all Horn formulas with bodies in canonical order
% to break symmetries irrelevant for testing provers
% of depth at most 3, as deeper ones can be reduced to these

allSortedHorn3(N,T):-
  succ(N,SN),length(Vs,SN),
  natpartitions(Vs),
  genSortedHorn3(N,T,Vs).
  
form2tuple(T,Tuple):-compound(T),!,functor(T,Op,N),
  atom_string(Op,OpStr),
  ( N=1->arg(1,T,X),
    form2tuple(X,A),
    Tuple=[OpStr,A]
  ; N=2->arg(1,T,X),arg(2,T,Y),
    form2tuple(X,A),
    form2tuple(Y,B),
    Tuple=[OpStr,A,B]
  ).
form2tuple(I,R):-integer(I),!,R=I.
form2tuple(A,S):-atom_string(A,S).
  
  
% all implicational logic formulas of size N
% A289679		a(n) = Catalan(n-1)*Bell(n).
allImpFormulas(N,T):-
  genTree(N,T,Vs),
  natpartitions(Vs).
 
  
allImpFormulas0(N,T):-
  genTree(N,T,Vs),
  maplist(=(o),Vs).
  
  
allNegImpFormulas(N,T):-
  genOpTree(N,[~,(->)],T,Vs),
  natpartitions(Vs).
 
  
%allHarropFormulas(N,T):-hdef(N,T,_).
allHarropFormulas(N,T):-hgoal(N,T,_).
  

% all classical implicational formulas  
allClassFormulas(N,T):-
  genTree(N,T,Vs),
  vpartitions(Vs),
  classvars(Vs).
  
% all Glivenko tranformed classic formulas  
allClassFormulas(N,T,NNT):-
  genTree(N,T,Vs),
  vpartitions(Vs),
  dneg(T,NNT),
  natvars(Vs).

allEqImplFormulas(N,T):-
  genOpTree(N, [(->),(<->)], T, Vs),
  natpartitions(Vs).
  

allNestedFormulas(N,T):-
  genOpTree(N, [
      (->)
      ,(&)
      ,(<->)
      ,(~)
    ], T, Vs),
  natpartitions(Vs).  

allEqNegFormulas(N,T):-
  genOpTree(N, [
      (<->)
      ,(~)
    ], T, Vs),
    natpartitions(Vs).  

  
allFullFormulas(N,T):-
  genOpTree(N,T,Vs),
  natpartitions(Vs).

  
 
allSortedFullFormulas(N,T):-
  genSortedTree(N,T,Vs),
  natpartitions(Vs).

  
  
allTrimmedFormulas(N,T):-
  genTrimmedTree(N,T,Vs),
  natpartitions(Vs).

  

unInhabitableTree(N,T):-
  genSortedHorn(N,T,Vs),
  \+ (
    natpartitions(Vs),
    hprove(T)
  ).
  
unInhabitableVars(N,Vs):-N>0,
  N1 is N-1,
  vpartitions(N,Vs),natvars(Vs),
  \+ (
    genSortedHorn(N1,T,Vs),
    hprove(T)
  ).  
    
% X combinator expressions

xType((((A->B->A)->((C->D->E)->(C->D)->C->E)->(F->G->F)->H)->H),[A,B,C,D,E,F,G,H]).

xType(T):-xType(T,_).

genX(N,X,T):-genX(X,T,N,0).

genX(x,T)-->{xType(T)}.
genX((A*B),T)-->pred,
  genX(A,ST),
  genX(B,S),
  {unify_with_occurs_check(ST,(S->T))}. 
 
  
  
  