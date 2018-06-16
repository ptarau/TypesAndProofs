% G4IP in Prolog
% https://www.andrew.cmu.edu/course/15-317/recitations/g4ip.pl
% adapted to share  notation with others - Paul Taray
% times out on ntest(7,g4prove) and ptest(12,g4prove)
% also very slow on others

%:- op(840, xfy, ->).   % implies, right assoc
%:- op(830, xfy,  v ).   % or, right assoc
%:- op(820, xfy, &).   % and, right assoc
%:- op(800,  fy, ).    % atom, prefix

% Top-level predicates.

g4prove(A) :-
  seqR([], [], A).

g4refute(A) :-
  \+ seqR([], [], A). % negation as failure

% Implementation.

% break down asynchronous propositions first -- right then left
%   G = context of synchronous props
%   O = context of props not yet processed
% choose synchronous propositions -- right then left
%   G = context of synchronous props not yet processed
%   H = context of unused synchronous propositions

% breaking asynchronous things down on the right
seqR(O, G, A & B) :-
  seqR(O, G, A),
  seqR(O, G, B).
seqR(O, G, A -> B) :-
  seqR([A | O], G, B).
seqR(_, _, tt).

% synchronous prop encountered on the right -- switching to the left
seqR(O, G, A  v  B) :-
  seqL(O, G, A  v  B).
seqR(O, G, false) :-
  seqL(O, G, false).
seqR(O, G, A) :- atomic(A),
  seqL(O, G, A).

% breaking asynchronous things down on the left
seqL([A & B | O], G, C) :-
  seqL([A,B | O], G, C).
seqL([A  v  B | O], G, C) :-
  seqL([A | O], G, C),
  seqL([B | O], G, C).
seqL([tt | O], G, C) :-
  seqL(O, G, C).
seqL([false | _], _, _).
seqL([(D & E) -> F | O], G, C) :-
  seqL([D -> (E -> F) | O], G, C).
seqL([tt -> F | O], G, C) :-
  seqL([F | O], G, C).
seqL([false -> _ | O], G, C) :-
  seqL(O, G, C).
seqL([(D  v  E)->F | O], G, C) :-
  seqL([D->F, E->F | O], G, C).

% synchronous left encountered -- move to gamma context
seqL([A -> D | O], G, C) :-atomic(A),
  seqL(O, [(A) -> D | G], C).
seqL([(D -> E) -> F | O], G, C) :-
  seqL(O, [(D -> E) -> F | G], C).
seqL([A | O], G, C) :-atomic(A),
  seqL(O, [A | G], C).

% context has been processed -- choose a synchronous rule
seqL([], G, C) :- chooseR(G, C).
seqL([], G, C) :- chooseL(G, [], C).

% break down synchronous prop on the right
chooseR(G, A  v  _B) :- seqR([], G, A).
chooseR(G, _A  v  B) :- seqR([], G, B).
chooseR(G, P) :- atomic(P),memberchk(P, G).
% chooseR(G, false) :- fail. % to force a goal to fail, don't include a rule.

% break down synchronous prop on the left
chooseL([P | G], H, C) :-atomic(P),
  chooseL(G, [P | H], C).
chooseL([P -> B | G], H, C) :-atomic(P),
  append(G, H, I),atomic(P),
  memberchk(P, I),
  !,
  seqL([B], I, C).
chooseL([P -> B | G], H, C) :-atomic(P),
  chooseL(G, [P -> B | H], C).
chooseL([(D -> E) -> B | G], H, C) :-
  append(G, H, I),
  seqR([E -> B, D], I, E),
  seqL([B], I, C).
chooseL([(D -> E) -> B | G], H, C) :-
  chooseL(G, [(D -> E) -> B | H], C).
% chooseL([], H, C) :- fail.

% Tests.

% prove( a -> a ).
% prove( a -> (b -> a) ).
% prove( (a -> b) -> (a -> (b -> c)) -> (a -> c) ).
% prove( a & b -> b & a ).
% prove( a  v  b -> b  v  a ).
% prove( (a  v  c) & (b -> c) -> (a -> b) -> c ).
% refute( (a -> b  v  c) -> (a -> b)  v  (a -> c) ).
% prove( ((a -> b)  v  (a -> c)) -> (a -> b  v  c) ).
% refute( ((a -> b) -> c) -> ((a  v  b) & (b -> c)) ).
% prove( ((a  v  b) & (b -> c)) -> ((a -> b) -> c) ).
% prove( (a -> b) -> (b -> c) -> (c -> d) -> (a -> d) ).
% prove( (a -> b) -> (a -> c) -> a -> b ).
% prove( (a -> b) -> (a -> c) -> a -> c ).
% prove( a -> (a -> b) -> (a -> c) -> b ).
% prove( a -> (a -> b) -> (a -> c) -> c ).
% prove( (a -> b -> c) -> a -> b -> c ).
% prove( (a -> b -> c) -> b -> a -> c ).
% prove( a -> b -> (a -> b -> c) -> c ).
% prove( b -> a -> (a -> b -> c) -> c ).
% prove( (a -> b) -> a -> b ).
% prove( ((a -> b) -> c) -> ((a -> b) -> c) ).
% prove( (((a -> b) -> c) -> d) -> (((a -> b) -> c) -> d) ).
% prove( ((((a -> b) -> c) -> d) -> e)
%                        -> (((a -> b) -> c) -> d) -> e ).
% prove( (((((a -> b) -> c) -> d) -> e) -> f)
%                        -> ((((a -> b) -> c) -> d) -> e) -> f ).
% prove( (((((a -> b) -> c) -> d) -> e) -> f)
%                       -> (((((a -> b) -> c) -> d) -> e) -> f)
%                        v  (((((a -> b) -> c) -> d) -> e) -> f) ).
% prove( ((a -> b) -> c) -> d -> d  v  d ).

