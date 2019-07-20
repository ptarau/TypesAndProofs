:-op(200,fy,k).
:-op(200,fy,m).

expand_k(false,R) :-!,R=false.
expand_k(true,R) :-!,R=true.
expand_k(A,R) :-atomic(A),!,R= A.
expand_k(~(A),(B->false)) :-!,expand_k(A,B).
expand_k(m(A),R):-!,expand_k(~k (~A),R).
expand_k(k(X),R) :-!,
  %T=(~ ~ X),
  T=((~ ~X-> X)-> X),
  expand_k(T,R).
expand_k(A,B) :-
  A=..[F|Xs],
  expand_ks(Xs,Ys),
  B=..[F|Ys].


expand_ks([],[]).
expand_ks([X|Xs],[Y|Ys]) :-expand_k(X,Y),expand_ks(Xs,Ys).

kaprove(T0) :-
  expand_k(T0,T1),
  ljfa(T1,[]).
  
  
% some theorems
axk(a->k a).
axk(k (a->b)->(k a->k b)).
axk(k a -> ~ ~a).

thk(T) :-axk(T).
thk(k p->k k p).
thk(~ k p -> k   (~k p)).
thk(~k p->k  (~k p)).
thk(k   (a & b) <-> (k a & k b)).
thk(~ k false).
thk(~ (k a & ~ a)).
thk(~a -> ~ k a).
thk( ~ ~ (k a -> a)).
% some other
thk(k a & k (a->b) -> k b).
thk(k a -> m a).
thk(~ k (~ a) <-> m a).
thk(m (a & b) <-> (m a & m b)).
thk(m a -> k a).
thk(k a -> a).
thk(k (a v b) -> k a v k b).

kgo:-thk(T),write(T),
   (kaprove(T)->write(' :: proven');true),nl,fail;true.