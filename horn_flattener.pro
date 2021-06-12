name_vars(T):-ground(T),!.
name_vars(T):-
  term_variables(T,Vs),
  length(Vs,L),L1 is L-1,
  numlist(0,L1,Ns),
  maplist(num2name,Ns,Vs).

num2name(Num,Name):-atom_concat('x__',Num,Name).

flat_horn(C,[D|Fs]):-
  flat_horn(C,D,Fs,[]),
  name_vars([D|Fs]).

flat_horn((H:-Bs),(H:-Rs))-->flat_horns(Bs,Rs).

flat_horns([],[])-->[].
flat_horns([B|Bs],[B|Rs])-->{atomic(B)},!,flat_horns(Bs,Rs).
flat_horns([C|Bs],[X|Rs])-->[X=D],flat_horn(C,D),flat_horns(Bs,Rs).

flatter_horn(C,Ds):-
 flat_horn(C,Cs),
 split_eqs(Cs,Ds,[]).

split_eqs([])-->[].
split_eqs([X=(H:-Bs)|Es])-->!,
  [(H:-[X|Bs])],[(X:-[(H:-Bs)])],
  split_eqs(Es).
split_eqs([X|Xs])-->[X],
  split_eqs(Xs).
