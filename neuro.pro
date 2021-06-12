:-ensure_loaded('horn_flattener.pro').

toHorn((A->B),(H:-Bs)):-!,toHorns((A->B),Bs,H).
toHorn(H,H).

toHorns((A->B),[HA|Bs],H):-!,toHorn(A,HA),toHorns(B,Bs,H).
toHorns(H,[],H).

ppp(X):-portray_clause(X).

fgo:-
  X=(a:-[b,(c:-[d,e,(q:-[p,r,s])]),f]),
  ppp(X),
  flat_horn(X,Y),
  ppp(Y),
  flatter_horn(X,Z),
  ppp(Z).

% works on Horn clauses - includes
% preprocessing from implicational form
% from which the translation is reversible



hprove(T0):-toHorn(T0,T),ljh(T).

ljh(A):-ljh(A,[]).

ljh(A,Vs):-memberchk(A,Vs),!.
ljh((B:-As),Vs1):-!,append(As,Vs1,Vs2),ljh(B,Vs2).
ljh(G,Vs1):- % atomic(G), G not on Vs1
  memberchk((G:-_),Vs1), % if not, we just fail
  select((B:-As),Vs1,Vs2), % outer select loop
  select(A,As,Bs),         % inner select loop
  ljh_imp(A,B,Vs2), % A element of the body of B
  !,
  trimmed((B:-Bs),NewB), % trim off empty bodies
  ljh(G,[NewB|Vs2]).

ljh_imp((D:-Cs),B,Vs):- !,ljh((D:-Cs),[(B:-[D])|Vs]).
ljh_imp(A,_B,Vs):-memberchk(A,Vs).

trimmed((B:-[]),R):-!,R=B.
trimmed(BBs,BBs).


bprove(T):-ljb(T,[]).

ljb(A,Vs):-memberchk(A,Vs),!.
ljb((A->B),Vs):-!,ljb(B,[A|Vs]).
ljb(G,Vs1):-
  select((A->B),Vs1,Vs2),
  ljb_imp(A,B,Vs2),
  !,
  ljb(G,[B|Vs2]).

ljb_imp((C->D),B,Vs):-!,ljb(D,[C,(D->B)|Vs]).
ljb_imp(A,_,Vs):-memberchk(A,Vs).



% derived directly from Dyckhoff's LJT calculus
lprove(T):-ljt(T,[]),!.

ljt(A,Vs):-memberchk(A,Vs),!.

ljt((A->B),Vs):-!,ljt(B,[A|Vs]).

ljt(G,Vs1):- %atomic(G),
  select((A->B),Vs1,Vs2),
  memberchk(A,Vs2),
  !,
  ljt(G,[B|Vs2]).

ljt(G,Vs1):- % atomic(G),
  select( ((C->D)->B),Vs1,Vs2),
  ljt((C->D), [(D->B)|Vs2]),
  !,
  ljt(G,[B|Vs2]).

% lemma
go:-
  X=((g -> ((c->d)->b) -> (c->d))) ,
  Y= (g -> (d->b) ->(c->d)),
  bprove(X->Y),
  bprove(Y->X).

go1:-
  X= (((c->d)->b) -> (c->d)) ,
  Y= ( (d->b) ->    (c->d)),
  bprove(X->Y),
  bprove(Y->X).

go2:-
  X=(((c->d)->b) -> c -> d) ,
  Y= ( (d->b) -> c -> d),
  bprove(X->Y),
  bprove(Y->X).

go3:-
  X=(g->((c->d)->b) -> c -> d) ,
  Y= (g-> (d->b) -> c -> d),
  bprove(X->Y),
  bprove(Y->X).

go4:-
  X= (c-> g-> ((c->d)->b) -> d) ,
  Y= (c-> g-> (d->b) -> d),
  bprove(X->Y),
  bprove(Y->X).

go5:-
  X= (c-> ((c->d)->b) -> d) ,
  Y= (c->  (d->b) -> d),
  bprove(X->Y),
  bprove(Y->X).

hgo:-
  X=((((c->d)->b) -> (c->d))-> (b->g)),
  Y0= ((c->  (d->b) -> d)-> (b->g)),
  Y=(((d->p) -> (p->b) -> (c->p)) -> (b->g)),

  bprove(Y->X), % equiprovable?
  bprove(Y0->X),
  bprove(Y->Y0),
  bprove(X->Y0),
  true.
