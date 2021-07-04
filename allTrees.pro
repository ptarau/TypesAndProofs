% Generates all trees of with N internal nodes and
% it  collects their leaves to a list of logic variables

genTree(N,Tree,Leaves):-genTree(Tree,N,0,Leaves,[]).

genTree(V,N,N)-->[V].
genTree((A->B),SN1,N3)-->{SN1>0,N1 is SN1-1},
  genTree(A,N1,N2),
  genTree(B,N2,N3).

% OEIS A000108 Catalan 1,2,5,14,42,132,429,1430,4862,16796 
genHorn(N,Tree,Leaves):-genHorn(Tree,N,0,Leaves,[]).

genHorn(V,N,N)-->[V].
genHorn((A:-[B|Bs]),SN1,N3)-->{succ(N1,SN1)},
  [A],
  genHorn(B,N1,N2),
  genHorns(Bs,N2,N3).
  
genHorns([],N,N)-->[].
genHorns([B|Bs],SN1,N3)-->{succ(N1,SN1)},
  genHorn(B,N1,N2),
  genHorns(Bs,N2,N3).

  
% A105633: [1,2,4,9,22,57,154,429,1223,3550,10455,31160,93802,284789]
%% canonicalized Horn clauses
%% one repres. of each equiv. class
genSortedHorn(N,Tree,Leaves):-
  succ(N,SN),length(Leaves,SN),
  generateSortedHorn(Tree,Leaves,[]).

generateSortedHorn(V)-->[V].
generateSortedHorn((A:-[B|Bs]))-->
  [A],
  generateSortedHorn(B),
  generateSortedHorns(B,Bs).
  
generateSortedHorns(_,[])-->[].
generateSortedHorns(B,[C|Bs])-->
  generateSortedHorn(C),
  {B@<C},
  generateSortedHorns(C,Bs).
  
  

% A004111		Number of rooted identity trees with n
% ?- countGen2(genSetTree,12,R).
% R = [1,1,2,3,6,12,25,52,113,247,548,1226].

genSetTree(N,Tree):-genSetTree(Tree,N,0).
  
genSetTree([],N,N).
genSetTree([B|Bs],SN1,N3):-succ(N1,SN1),
  genSetTree(B,N1,N2),
  genSetTrees(B,Bs,N2,N3).
  
genSetTrees(_,[],N,N).
genSetTrees(B,[C|Bs],SN1,N3):-succ(N1,SN1),
  genSetTree(C,N1,N2),
  B@<C,
  genSetTrees(C,Bs,N2,N3).
  

% hereditarily finite sets generators

nat2nats(0,[]).
nat2nats(N,Ns):-N>0,findall(I,nat2bit(N,I),Ns).

nat2bit(N,I):-
  M is msb(N),
  between(0,M,I),
  1 is getbit(N,I).

nat2hfs(0,[]).
nat2hfs(N,Hs):-N>0,
  nat2nats(N,Ns),
  maplist(nat2hfs,Ns,Hs).
  
nats2nat([],0).
nats2nat([I|Ns],N):-
  E is 1<<I,
  nats2nat(Ns,N1),
  N is N1+E.

hfs2nat(Ns,N):-
  maplist(hfs2nat,Ns,Ms),
  nats2nat(Ms,N).  
  
% will be distinct from genSortedHorn only when Leaves are bound  
genStrictHorn(N,Tree,Leaves):-
  genStrictHorn(Tree,N,0,Leaves,[]).
  
genStrictHorn(V,N,N)-->[V].
genStrictHorn((H:-[B|Bs]),SN1,N3)-->{succ(N1,SN1)},
  [H],
  genStrictHorn(B,N1,N2),
  genStrictHorns(B,Bs,N2,N3),
  {\+ ((member(X,[B|Bs]),X==H))}.
  
genStrictHorns(_,[],N,N)-->[].
genStrictHorns(B,[C|Bs],SN1,N3)-->{succ(N1,SN1)},
  genStrictHorn(C,N1,N2),
  {B@<C},
  genStrictHorns(C,Bs,N2,N3).
  
  
  
genSortedHorn3(N,Tree,Leaves):-
  genSortedHorn3(3,Tree,N,0,Leaves,[]).

genSortedHorn3(_,V,N,N)-->[V].
genSortedHorn3(SK,(A:-[B|Bs]),SN1,N3)-->{succ(N1,SN1),succ(K,SK)},
  [A],
  genSortedHorn3(K,B,N1,N2),
  genSortedHorn3s(SK,B,Bs,N2,N3).
  
genSortedHorn3s(_,_,[],N,N)-->[].
genSortedHorn3s(K,B,[C|Bs],SN1,N3)-->{succ(N1,SN1)},
  genSortedHorn3(K,C,N1,N2),
  {B@<C},
  genSortedHorn3s(K,C,Bs,N2,N3).

  
genHorn3(N,Tree):-
  genHorn3(3,Tree,N,0).

genHorn3(_,_V,N,N).
genHorn3(SK,(_A:-[B|Bs]),SN1,N3):-succ(N1,SN1),succ(K,SK),
  genHorn3(K,B,N1,N2),
  genHorn3s(SK,B,Bs,N2,N3).
  
genHorn3s(_,_,[],N,N).
genHorn3s(K,_B,[C|Bs],SN1,N3):-succ(N1,SN1),
  genHorn3(K,C,N1,N2),
  genHorn3s(K,C,Bs,N2,N3).

  
genOpTree(N,Tree,Leaves):-genOpTree(N,[(~),(->),(<->),(&),(v)],Tree,Leaves).

genOpTree(N,Ops0,Tree,Leaves):-
  select((~),Ops0,Ops),
  !,
  genNegTree(Ops,Tree,N,0,Leaves,[]).
genOpTree(N,Ops,Tree,Leaves):-
  genTree(Ops,Tree,N,0,Leaves,[]).

genTree(_,V,N,N)-->[V].
genTree(Ops,OpA,SN1,N2)-->
  { SN1>0,N1 is SN1-1,
    member(Op/1,Ops),make_op1(Op,A,OpA)
  },
  genTree(Ops,A,N1,N2).
genTree(Ops,OpAB,SN1,N3)-->
  { SN1>0,N1 is SN1-1,
    member(Op,Ops),atomic(Op), % by default, Ops have arity 2
    make_op(Op,A,B,OpAB)
  },
  genTree(Ops,A,N1,N2),
  genTree(Ops,B,N2,N3).
  
genNegTree(_,NegV,N1,N2)-->[V],{add_neg_ops(V,NegV,N1,N2)}.
genNegTree(Ops,OpA,SN1,N2)-->
  { SN1>0,N1 is SN1-1,
    member(Op/1,Ops),make_op1(Op,A,OpA)
  },
  genNegTree(Ops,A,N1,N2).
genNegTree(Ops,NegOpAB,SN1,N4)-->
  { SN1>0,N1 is SN1-1,
    member(Op,Ops),atomic(Op),make_op(Op,A,B,OpAB)
  },
  genNegTree(Ops,A,N1,N2),
  genNegTree(Ops,B,N2,N3),
  {add_neg_ops(OpAB,NegOpAB,N3,N4)}.
 
make_op1(Op,A,OpA):-functor(OpA,Op,1),arg(1,OpA,A).  

make_op(Op,A,B,OpAB):-functor(OpAB,Op,2),arg(1,OpAB,A),arg(2,OpAB,B).


add_neg_ops(OpAB,OpAB,N,N).
add_neg_ops(OpAB,~OpAB,SN,N):-succ(N,SN).
add_neg_ops(OpAB, ~ ~OpAB,SSN,N):-SSN>1,N is SSN-2.

genSortedTree(N,Tree,Leaves):-
   genSortedTree(N,[(~),(->)],[(<->),(&),(v)],
   Tree,Leaves).

genSortedTree(N,Ops,Cops,Tree,Leaves):-
  genSortedTree(Ops,Cops,Tree,N,0,Leaves,[]).

genSortedTree(_,_,V,N,N)-->[V].
genSortedTree(Ops,Cops,~A,SN1,N2)-->
  {memberchk((~),Ops),SN1>0,N1 is SN1-1},
  genSortedTree(Ops,Cops,A,N1,N2).
genSortedTree(Ops,Cops,OpAB,SN1,N3)-->
  {SN1>0,N1 is SN1-1,
    member(Op,Ops),Op\=(~),make_op(Op,A,B,OpAB)
  },
  genSortedTree(Ops,Cops,A,N1,N2),
  genSortedTree(Ops,Cops,B,N2,N3).
genSortedTree(Ops,Cops,OpAB,SN1,N3)-->
  {SN1>0,N1 is SN1-1,member(Op,Cops),make_op(Op,A,B,OpAB)},
  genSortedTree(Ops,Cops,A,N1,N2),
  genSortedTree(Ops,Cops,B,N2,N3),
  {A@<B}.
  

genTrimmedTree(N,Tree,Leaves):-
   genTrimmedTree(N,[(->)],[(<->),(&),(v)],Tree,Leaves).

genTrimmedTree(N,Ops,Cops,Tree,Leaves):-
  genTrimmedTree(Ops,Cops,Tree,N,0,Leaves,[]). 

%genTrimmedTree(_,_,T,N,_)-->{ppp(N:T),fail}.
genTrimmedTree(_,_,V,N1,N2)-->[V0],
  {add_neg_ops(V0,V,N1,N2)}.
genTrimmedTree(Ops,Cops,OpAB,SN1,N4)-->
  { SN1>0,N1 is SN1-1,
    member(Op,Ops),make_op(Op,A,B,OpAB0),
    add_neg_ops(OpAB0,OpAB,N1,N2)
  },
  genTrimmedTree(Ops,Cops,A,N2,N3),
  genTrimmedTree(Ops,Cops,B,N3,N4).
genTrimmedTree(Ops,Cops,OpAB,SN1,N4)-->
  { SN1>0,N1 is SN1-1,
    member(Op,Cops),
    make_op(Op,A,B,OpAB0),
    add_neg_ops(OpAB0,OpAB,N1,N2)
  },
  genTrimmedTree(Ops,Cops,A,N2,N3),
  genTrimmedTree(Ops,Cops,B,N3,N4),
  {A@<B}.  

  
fsize(A,S):-primitive(A),!,S=0.  
fsize(A,S):-functor(A,F,_N),A=..[F|Xs],
  maplist(fsize,Xs,Rs),
  sum_list([1|Rs],S).
  
  
  
  
  
  
  
