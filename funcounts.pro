% counts function syms and constants in aterm
% note: it collapses vars and nums as if $var/0 $num/0 constants
funcount(T,Counts):-
  in_term(T,FNs),
  map_pair_sums(FNs,Counts).
  
map_pair_sums(FNs,Counts):-  
  keysort(FNs,MSet),
  group_pairs_by_key(MSet,Grouped),
  maplist(sum_counts,Grouped,Counts).
  
sum_counts(F/N-Ks,F/N-S):-sumlist(Ks,S). 

in_term(T,Fs):-in_term(T,Fs,[]).

% collects all function syms and constants in aterm
in_term(T)-->{
     compound(T),
     functor(T,F,N),
     T=..[F|Xs]
   },
   [F/N-1],
   in_terms(Xs).
in_term(T)-->{atom(T)},[T/0-1].
in_term(T)-->{number(T)},['$number'/0-1].
in_term(T)-->{var(T)},['$var'/0-1].
   
in_terms([])-->[].
in_terms([X|Xs])-->in_term(X),in_terms(Xs).
  
sum_counts(S,Gen,Counts,Percs):-
  arg(1,Gen,S),
	arg(2,Gen,X),
  findall(FNs,(Gen,funcount(X,FNs)),Xss),
  append(Xss,FNs),
  map_pair_sums(FNs,Counts),
  perc_counts(Counts,Percs).

% turns counts into percentages
perc_counts(Counts,Percs):-
  maplist(arg(2),Counts,Ks),
  sumlist(Ks,Sum),
  maplist(div_with(Sum),Ks,Ds),
  maplist(add_perc,Counts,Ds,Percs).
  
add_perc(FN-_,D,FN-D).  
 

div_with(0,X,X/0).
div_with(N,X,R):-N =\=0, R is X/N.

% counts function syms and constants in all ansers of a generator Gen
nct(N,Gen):-
  sum_counts(N,Gen,Counts,Percs),
  writeln(Counts),writeln(Percs),fail
; true.

% counts when Gen works in successor arithmetic
sct(N,Gen):-n2s(N,S),nct(S,Gen),fail;true.



