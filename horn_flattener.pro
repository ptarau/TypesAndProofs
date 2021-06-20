% main entry - ensures no nesting deeper than 3 - a Tseitin-like form
% then, the tree, as the set of all paths can fit into a nice matrix
% then, recognising theorems becomes a  classification problem
% for simple 2D images - possibly good to train a convolutional NN

flatter_horn(C,D):-
  flat_horn(C,D),
  max_depth(D,M),
  assertion(M<4).

flat_horn(T,R):-max_depth(T,3),!,R=T.
flat_horn((H:-Xs),Res):-
  flat_horn(2,(H:-Xs),(H:-Bs),Fs,Bs),
  name_vars(Fs),
  to_strict((H:-Fs),Res).

flat_horn(K,T,R)-->{max_depth(T,K)},!,{R=T}.
flat_horn(K,(H:-Bs),(H:-Rs))-->{K1 is K-1},flat_horns(K1,Bs,Rs).

flat_horns(_,[],[])-->[].
flat_horns(K,[B|Bs],[B|Rs])-->{atomic(B)},!,flat_horns(K,Bs,Rs).
flat_horns(K,[C|Bs],[X|Rs])--> %[X=D],
  flat_horn(K,C,D),
  split_eq(K,X=D),
  flat_horns(K,Bs,Rs).

%split_eq(_,(X=D))-->{atomic(D)},!,{X=D}.
split_eq(K,(X=D))-->{K>= 0},!,{X=D}.
split_eq(_,(X=(H:-Bs)))-->[(H:-[X|Bs]),(X:-[(H:-Bs)])].

to_sorted(X,R):-atomic(X),!,R=X.
to_sorted((H:-Bs),(H:-Xs)):-
  maplist(to_sorted,Bs,Cs),
  sort(Cs,Xs).

to_strict(X,R):-atomic(X),!,R=X.
to_strict((H:-Bs),Strict):-
  maplist(to_strict,Bs,Cs),
  sort(Cs,Xs),
  trim_to_strict((H:-Xs),Strict).

trim_to_strict((H:-Xs),(H:-[H])):-memberchk(H,Xs),!.
trim_to_strict(C,C).

name_vars(T):-ground(T),!.
name_vars(T):-
  term_variables(T,Vs),
  length(Vs,L),L1 is L-1,
  numlist(0,L1,Ns),
  maplist(num2name,Ns,Vs).

max_depth(X,R):-atomic(X),!,R=0.
max_depth(_:-Bs,R):-
  maplist(max_depth,Bs,Ds),
  sort(Ds,Rs),
  last(Rs,R1),
  R is 1+R1.

num2name(Num,Name):-atom_concat('x',Num,Name).

consts_of(T,Xs):-const_of(T,Cs,[]),sort(Cs,Xs).

const_of(A)-->{atomic(A)},!,[A].
const_of((H:-Bs))-->[H],consts_of(Bs).

consts_of([])-->[].
consts_of([B|Bs])-->const_of(B),consts_of(Bs).


fttest:-
  % ttest if replacing any tautology
  % with p:-[p] is equi-provable

  T=(0:-[1,2,(0:-[(3:-[0,3,(4:-[0])])])]),
  S=(6:-[6]),

  I1=(p:-[T,q]),
  I2=(p:-[S,q]),
  J1=(I1:-[I2]),
  J2=(I2:-[I1]),

  II1=(T:-[p,q]),
  II2=(S:-[p,q]),
  JJ1=(II1:-[II2]),
  JJ2=(II2:-[II1]),

  hprove(J1),
  hprove(J2),
  hprove(JJ1),
  hprove(JJ2).

