name_vars(T):-ground(T),!.
name_vars(T):-
  term_variables(T,Vs),
  length(Vs,L),L1 is L-1,
  numlist(0,L1,Ns),
  maplist(num2name,Ns,Vs).

is_kdeep(0,(_:-_)) :- !.
is_kdeep(K,(_:-Bs)):-K>0,K1 is K-1,member(B,Bs),is_kdeep(K1,B),!.

num2name(Num,Name):-atom_concat('x__',Num,Name).

flat_horn((H:-Xs),(H:-Fs)):-
  %assertion(Xs=[_|_]),
  flat_horn(2,(H:-Xs),(H:-Bs),Fs,Bs),
  name_vars(Fs).

flat_horn(K,T,R)-->{\+is_kdeep(K,T)},!,{R=T}.
flat_horn(K,(H:-Bs),(H:-Rs))-->flat_horns(K,Bs,Rs).

flat_horns(_,[],[])-->[].
flat_horns(K,[B|Bs],[B|Rs])-->{atomic(B)},!,flat_horns(K,Bs,Rs).
flat_horns(K,[C|Bs],[X|Rs])-->[X=D],{K1 is K-1},
  flat_horn(K1,C,D),
  flat_horns(K,Bs,Rs).

flatter_horn(C,(H:-Xs)):-
 flat_horn(C,(H:-Bs)),
 sort(Bs,Cs),
 split_eqs(Cs,Ds,[]),
 sort(Ds,Xs).

split_eqs([])-->[].
split_eqs([X=(H:-As)|Es])-->!,
  {sort(As,Bs),sort([X|Bs],Cs)},
  [(H:-Cs)],[(X:-[(H:-Bs)])],
  split_eqs(Es).
split_eqs([X|Xs])-->[X],
  split_eqs(Xs).
