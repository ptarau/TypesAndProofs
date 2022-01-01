% dictionary-based provers

hdprove(T0):-toDHorn(T0,T),ljd(T).

ljd(A):-ljd(A,[]).

ljd(A,Vs):-memberchk(A,Vs),!.
ljd((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljd(B,Vs2).
ljd(G,Vs1):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  ljd_imp(A,B,Vs2), % A element of the body of B
  !,
  d_trimmed((B:-Bs),NewB), % trim off empty bodies
  ljd(G,[NewB|Vs2]).

ljd_imp((D:-Cs),B,Vs):- !,ljd((D:-Cs),[(B:-[D])|Vs]).
ljd_imp(A,_B,Vs):-memberchk(A,Vs).

d_trimmed((B:-[]),R):-!,R=B.
d_trimmed(BBs,BBs).


terms2pairs([],[]).
terms2pairs([T|Ts],[P|Ps]):-
  term2pairs(T,P),
  terms2pairs(Ts,Ps).

term2pairs(A,R):-(atomic(A);var(A)),!,R=(A:-[]).
term2pairs(T,R):-
  T=..[F|Xs],
  terms2pairs(Xs,Ps),
  R=(F:-Ps).

% only makes sense assuming all function syms distinct at a given level
to_dict(Xs,D):-
  terms2dict(Xs,Ps),
  dict_create(D,'',Ps).


terms2dict([],[]).
terms2dict([T|Ts],[P|Ps]):-
  term2dict(T,P),
  terms2dict(Ts,Ps).

term2dict(A,R):-(atomic(A);var(A)),!,R=A-[].
term2dict(T,R):-
  T=..[F|Xs],
  to_dict(Xs,D),
  R=F-D.


hd1:-
  terms2pairs([h(c),f(a,g(b,h(c,d)),d)],R),
  writeln(R).

hd2:-
  to_dict([h(c),f(a,g(b,c),d)],R),
  writeln(R).
