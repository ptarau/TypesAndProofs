% factor a number
factor(N,Ns) :- N > 0,  prime_factors(N,Ns,2).

prime_factors(1,[],_) :- !.
prime_factors(N,[F|L],F) :-
   R is N // F, N =:= R * F,
   !,
   prime_factors(R,L,F).
prime_factors(N,L,F) :- 
   next_factor(N,F,NF),
   prime_factors(N,L,NF).

next_factor(_,2,3) :- !.
next_factor(N,F,NF) :- F * F < N, !, NF is F + 2.
next_factor(N,_,N).

add_one(X,X-1).

add_len(X-[1],R):-!,R=X.
add_len(X-Xs,X^L):-length(Xs,L).

% canonical form as in the Fund. theor. of Arithmetic

to_canonical_factors(N,Es):-
  factor(N,Ps),
  to_canonical(Ps,Es).

to_canonical(Xs,Cs):-
  maplist(add_one,Xs,Ps),
  group_pairs_by_key(Ps,PKs),
  maplist(add_len,PKs,Cs).

% turns natural number into
% list of nested horn clause formulas
to_ipc(N,Rs):-
   to_canonical_factors(N,Es),
   to_ipcs(Es,Rs).

to_ipcs([],[]).
to_ipcs([(B^A)|Xs],[(B:-As)|Ys]):-!,
  to_ipc(A,As),
  to_ipcs(Xs,Ys).
to_ipcs([A|Xs],[A|Ys]):-
  to_ipcs(Xs,Ys).

nat2horn(N,(H:-Bs)):-
   to_ipc(N,[H|Bs]).

nat2hard_horn(N,(H:-Bs)):-
   to_ipc(N,Xs),
   append(Bs,[H],Xs).
