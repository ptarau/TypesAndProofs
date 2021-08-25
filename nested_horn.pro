:-op(600,xfx,(<-)).
:-op(500,fy,(~)).


% prover for nested Horn + false + true - compiled from all except disjunction

holds(A):-A<-[].

%true<-_:-!.
A<-Vs:-memberchk(A,Vs),!.
_<-Vs:-memberchk(false,Vs),!.
(B<-As)<-Vs1:-!,append(As,Vs1,Vs2),B<-Vs2.
G<-Vs1:- % atomic(G), G not on Vs1
  membtest(G,Vs1),
  select((B<-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  holds_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B<-Bs),NewB), % trim empty bodies
  G<-[NewB|Vs2].
  
holds_imp((D<-Cs),B,Vs):-!,(D<-Cs)<-[(B<-[D])|Vs].
%holds_imp(true,_B,_Vs):-!.
holds_imp(A,_B,Vs):-memberchk(A,Vs).

membtest(G,Vs):-memberchk((G<-_),Vs),!. % if not, we just fail
membtest(_,Vs):-memberchk((false<-_),Vs). % could still be infered from false

trimmed((B<-[]),R):-!,R=B.
trimmed(BBs,BBs).

nholds(F):-neg_expand(F,NF),holds(NF).

holds_classicaly(F):-
  glivenko(F,G),
  nholds(G).


% abductive implicational IPC


subset_of(Xs,Ss):-
  length(Xs,L),
  between(0,L,K),
  ksubset(K,Xs,Ss).

% subsets of K elements of a set with N elements
ksubset(0,_,[]).
ksubset(K,[X|Xs],[X|Rs]):-K>0,K1 is K-1,ksubset(K1,Xs,Rs).
ksubset(K,[_|Xs],Rs):-K>0,ksubset(K,Xs,Rs).


leaves_of(Form,Leaves):-
  leaves_of(Form,Xs,[]),
  sort(Xs,Leaves).

leaves_of(H<-Bs)-->!,leaves_of(H),map_leaves(Bs).
leaves_of(X)-->[X].

map_leaves([])-->[].
map_leaves([X|Xs])-->leaves_of(X),map_leaves(Xs).

leaves_of_all([])-->[].
leaves_of_all([X|Xs])-->leaves_of(X),leaves_of_all(Xs).

%% abductive inference of atomic formulas in a formula
%% that must hold for the formula to hold in IPC
needed_for(Prover,Formula,Hypos):-
  % ex: Prover = (<-)
  Formula=(H<-_),
  leaves_of(Formula,Ls),
  selectchk(H,Ls,Others),
  subset_of(Others,Hypos),
  call(Prover,Formula,Hypos).




% nested Horn clause formula generators

nested_horn(N,T):-gen_formula(N,gen_horn,T).

trimmed_horn(N,T):-gen_formula(N,gen_trimmed_horn,T).

gen_formula(N,TreeGen,T):-
  length(Vs,N),
  natpartitions(Vs),
  call(TreeGen,T,Vs).


% set partition generator - counted by Bell numbers

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

% from set partitions, with 0..N marking distinct variables

natpartitions(Vs):-
   mpart_of(Vs,Ns),
   length(Ns,SL),
   succ(L,SL),
   numlist(0,L,Ns).


% sorted nested Horn clauses, with no repetitions

% OEIS A000108 Catalan 1,2,5,14,42,132,429,1430,4862,16796
gen_horn(Tree,Leaves):-gen_horn(Tree,Leaves,[]).

gen_horn(V)-->[V].
gen_horn((A<-[B|Bs]))-->
  [A],
  gen_horn(B),
  gen_horns(Bs).

gen_horns([])-->[].
gen_horns([B|Bs])-->
  gen_horn(B),
  gen_horns(Bs).


% A105633: [1,2,4,9,22,57,154,429,1223,3550,10455,31160,93802,284789]
%% canonicalized Horn clauses
%% one repres. of each equiv. class
gen_trimmed_horn(Tree,Leaves):-
  generate_trimmed_horn(Tree,Leaves,[]).

generate_trimmed_horn(V)-->[V].
generate_trimmed_horn((A<-[B|Bs]))-->
  [A],
  generate_trimmed_horn(B),
  generate_trimmed_horns(B,Bs).

generate_trimmed_horns(_,[])-->[].
generate_trimmed_horns(B,[C|Bs])-->
  generate_trimmed_horn(C),
  {B@<C},
  generate_trimmed_horns(C,Bs).

% transformers

% from/to implcational formulas

iform_horn(I,((H<-Cs)<-Bs)):-var(I),!,append(Cs,Bs,Ds),iform_horn(I,(H<-Ds)).
iform_horn((A->B),(H<-Bs)):-!,iform_horns((A->B),Bs,H).
iform_horn(H,H).

iform_horns((A->B),[HA|Bs],H):-!,iform_horn(A,HA),iform_horns(B,Bs,H).
iform_horns(H,[],H).

neg_expand(~H<-Bs,R):-!,maplist(neg_expand,[H|Bs],Cs),R = false<-Cs.
neg_expand(~(H<-Bs),R):-!,maplist(neg_expand,[H|Bs],[NH|NBs]),R = false<-(NH<-NBs).
neg_expand(H<-Bs,R):-!,maplist(neg_expand,[H|Bs],[NH|NBs]),R=(NH<-NBs).
neg_expand(~X,R):-!,neg_expand(X,NX),R=(false<-[NX]).
neg_expand(A,A).


% turns formula into equivalent to being classically provable
%glivenko(H<-Bs,R):-!,maplist(glivenko,[H|Bs],[G|Cs]),R = (G<-Cs).
%glivenko(false,R):-!,R=false.
glivenko(X,R):-R=(false<-[false<-[X]]).


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

hornify([X],(X->false)).
hornify([X,Y|Xs],(X<-[Y|Xs])).

% nested Horn clause -> natural number
uhorn2nat(U,U):-atomic(U).
uhorn2nat(HBs,N):-
  hornify(Hs,HBs),
  maplist(uhorn2nat,Hs,Ns),
  nats2nat(Ns,N).

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


% counts nb. of solutions of Goal
sols_count(Goal, Times) :-
        Counter = counter(0),
        (   Goal,
            inc(Counter),
            fail
        ;   arg(1, Counter, Times)
        ).

counts_for(M,Generator,Ks):-
  findall(K,
    (between(0,M,L),sols_count(call(Generator,L,_),K)),
    Ks).


ppp(X):-portray_clause(X).

inc(Ctr):-arg(1,Ctr,X),succ(X,Y),nb_setarg(1,Ctr,Y).

go1:-
   Ctr=proven(0),
   NCtr=unprovable(0),
   M=50000,
   ( between(0,M,N),
     N1 is floor(log(1+N)),
     between(0,N1,U),
     nat2uhorn(U,N,HBs),
     uhorn2nat(HBs,NN),
     assertion(N=:=NN),
     ( holds(HBs)->
         R=proven,inc(Ctr)
     ;   R=unprovable,inc(NCtr)
     ),
     0=:=random(M),0=:=random(U),ppp((N=NN):'------>':HBs:R),
     fail
   ; Ctr=proven(K),
     NCtr=unprovable(NK),
     D is K/(K+NK),
     writeln([Ctr,NCtr,density=D])
   ).

go:-
  trimmed_horn(10,T),
  holds_classicaly(T),not(holds(T)),
  once(needed_for((<-),T,Ls)),
  Ls=[_,_|_],
  ppp(T),
  ppp(Ls),nl,fail.
