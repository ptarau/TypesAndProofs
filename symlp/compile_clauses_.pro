:- multifile(term_expansion/2).

:-op(1199,xfx,(=>)).
:-op(1199,xfx,(<=)).

compile_clauses(C,_):-var(C),!,fail.

compile_clauses((H<=B),true:(H:-B)):-!,nonvar(H),nonvar(B).
compile_clauses((+H),true:H):-!,nonvar(H).

compile_clauses((H=>B),R):-nonvar(H),nonvar(B),!,dual2clause((H=>B),R).
compile_clauses((-H),false:H):-nonvar(H).

dual2clause((H=>false),false:(H)):-!.
dual2clause((H=>B),false:(H:-CB)):-disj2conj(B,CB).
dual2clause((-H),false:H).

disj2conj((A;B),(CA,CB)):-nonvar(A),nonvar(B),!,
    disj2conj(A,CA),
    disj2conj(B,CB).
disj2conj(A,A).


show:-
  writeln('% module true:'),
  listing(true:_),
  nl,
  writeln('% module false:'),
  listing(false:_),
  nl.

% activate compilation of mixed Horn and Dual Horn clause program
% that starts with :-include('compile_symlp.pro').

term_expansion(C,R) :- compile_clauses(C,R).

% USAGE:

% calling in the Horn program
% ?-true:X.

% calling in the Dual Horn prgram
% ?-false:X.

