% idea: mimic lazy grounding when calling prover
obprove(T):-oljb(T,[]),!.

%oljb(A,Vs):-ppp((Vs-->A)),fail. % fo traing only


oljb((A->B),Vs):-!,activate(A),oljb(B,[A|Vs]).
oljb(A,Vs):-memberchk(A,Vs),!.
oljb(G,Vs1):-activate(G),
  select((A->B),Vs1,Vs2),
  oljb_imp(A,B,Vs2),
  oljb(G,[B|Vs2]).

oljb_imp((C->D),B,Vs):-!,oljb((C->D),[(D->B)|Vs]).
oljb_imp(A,_,Vs):-activate(A),memberchk(A,Vs).   


grounder_def(E):-E=e(_,_),member(E,
   [e(r,g),e(r,b),e(b,g),e(b,r),e(g,r),ge(g,b)]).
   
% examples

/*
ge(r,g).
ge(r,b).
ge(b,g).
ge(b,r).
ge(g,r).
ge(g,b).
*/

ogoal_1(
  (v(r) & v(g) & v(b)) & (ge(X,X)->false) ->
  (ge(C1,C2) <-> v(C1) & v(C2))&
  (ge(C2,C3) <-> v(C2) & v(C3))&
  (ge(C1,C3) <-> v(C1) & v(C3))&
  (ge(C3,C4) <-> v(C3) & v(C4))&
  (ge(C2,C5) <-> v(C2) & v(C5))&
  (ge(C4,C5) <-> v(C4) & v(C5))&
  (ge(C5,C6) <-> v(C5) & v(C6))&
  (ge(C4,C6) <-> v(C4) & v(C6))&
  (ge(C1,C6) <-> v(C1) & v(C6))
).

ogoal_(
ge(r,g)&
ge(r,b)&
ge(b,g)&
ge(b,r)&
ge(g,r)&
ge(g,b)&
  (ge(C1,C2)&
  ge(C2,C3)&
  ge(C1,C3)&
  ge(C3,C4)&
  ge(C2,C5)&
  ge(C4,C5)&
  ge(C5,C6)&
  ge(C4,C6)&
  ge(C1,C6)->ok
  )->
  ok
  
).
ogo:-(ogoal_(G),ljfa(G,[])*->ppp(G);fail).

hgo:-(hgoal_(G),ljnh(G,[])*->ppp(G);fail).
hgo1:-(hgoal1_(G),ppp(G),ljnh(G,[])*->ppp(G);fail).


hgoal1_(g(C1,C2,C3,C4,C5,C6):-[
  (g(C1,C2,C3,C4,C5,C6):-[
     e(C1,C2),
     e(C2,C3),e(C1,C3),
     e(C3,C4),
     e(C4,C5),
     e(C2,C5),
     e(C5,C6),e(C4,C6),
     e(C1,C6)
    ]         
  ),
     e(C1,C2),
     e(C2,C3),e(C1,C3),
     e(C3,C4),
     e(C4,C5),
     e(C2,C5),
     e(C5,C6),e(C4,C6),
     e(C1,C6),
    (false:-[e(C1,C1)]),
    (false:-[e(C2,C2)]),
    (false:-[e(C3,C3)]),
    (false:-[e(C4,C4)]),
    (false:-[e(C5,C5)]),
    (false:-[e(C6,C6)])
]):-
   gounder_def([C1,C2,C3,C4,C5,C6]).

hgt1((g:-[
  g:-[e(C1,C2),(false:-[e(C1,C1)]), (false:-[e(C2,C2)])  ]
  ]
  )):-member(C1,[a,b]),member(C2,[a,b]).
  
hgt2((g:-[
    g:-[ea],(false:-eb),ea
  ])).
  
gounder_def(Cs):-
  maplist(ground_one,Cs).

ground_one(C):-member(C,[r,g,b]).  
  
hgoal_(g(C1,C2,C3,C4,C5,C6):-[
    (false:-[e(X,X)]),
  (g(C1,C2,C3,C4,C5,C6):-[
      c(C1),c(C2),e(C1,C2),
      c(C3),e(C2,C3),e(C1,C3),
      c(C4),e(C3,C4),
      c(C5),e(C2,C5),e(C4,C5),
      c(C6),e(C5,C6),e(C4,C6),e(C1,C6)
    ]
  ),
  c(r),
  c(g),
  c(b) /*
  e(r,g),
  e(r,b),
  e(b,g),
  e(b,r),
  e(g,r),
  e(g,b)*/
  
 
]).
  
