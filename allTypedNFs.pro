% generate all simply typed normal forms of given size

nfTypes(N,T):-tnf(N,_:T),natvars(T).

% typable normal form of size N
tnf(N,X:T):-n2s(N,S),tnf(X,T,[],S,0,Gs,true),Gs.

tnfs(N,X,T):-tnf(N,X,T,Gs,true),Gs.

tnf(N,X,T,Gs,Gs0):-
  n2s(N,S),
  tnf(X,T,[],S,_,Gs,Gs0).

tnf(X,P,[Q|Ps],N,N)-->add_hypo(X,P,[Q|Ps]).
tnf(l(X,A),(P->Q),Ps,s(N1),N2)--> 
  % introduce P, for proving P->Q
  tnf(A,Q,[X:P|Ps],N1,N2).  
tnf(a(A,B),Q,Ps,s(s(N1)),N3)--> 
  % try Modus Ponens for Q, by proving P->Q and P
  tnf0(A,(P->Q),Ps,N1,N2),
  tnf(B,P,Ps,N2,N3).

tnf0(X,P,[Q|Ps],N,N)--> add_hypo(X,P,[Q|Ps]).
tnf0(a(A,B),Q,Ps,s(s(N1)),N3)--> 
  % try Modus Ponens for Q, by proving P->Q and P
  tnf0(A,(P->Q),Ps,N1,N2),
  tnf(B,P,Ps,N2,N3).

add_hypo(X,P,Ps,(hypo(X,P,Ps),Gs),Gs).

hypo(X,P,Ps):-member(X:Q,Ps),unify_with_occurs_check(P,Q).


% computes type of an expression X
type_of(X,T):-type_of(X,T,[]).

type_of(X,T0,Vs):-var(X),!,
  member(X0:T,Vs),X==X0,
  unify_with_occurs_check(T0,T).
type_of(l(X,A),(S->T),Vs):-
  type_of(A,T,[X:S|Vs]).
type_of(a(A,B),T,Vs):-
  type_of(A,(S->T),Vs),
  type_of(B,S,Vs).


% alternative implementations

% typable closed normal form of size N and its type
typed_nf(N,X:T):-typed_nf(X,T,[],N,0).

pred(SX,X):-succ(X,SX).

typed_nf(l(X,E),(P->Q),Ps)-->pred,typed_nf(E,Q,[X:P|Ps]).  
typed_nf(X,P,Ps)-->typed_nf_no_left_lambda(X,P,Ps).

typed_nf_no_left_lambda(X,P,[Y:Q|Ps])--> agrees(X:P,[Y:Q|Ps]).
typed_nf_no_left_lambda(a(A,B),Q,Ps)-->pred,pred,
  typed_nf_no_left_lambda(A,(P->Q),Ps),
  typed_nf(B,P,Ps).

agrees(P,Ps,N,N):-member(Q,Ps),unify_with_occurs_check(P,Q).

% type of closed normal form of size N, with lambda term omitted
% formula known-for-sure: an implicational intuitionistic
% propositional tautology
impl_taut(N,T):-impl_taut(T,[],N,0).

impl_taut((P->Q),Ps)-->pred,impl_taut(Q,[P|Ps]).  
impl_taut(P,Ps)-->impl_taut_no_left_lambda(P,Ps).

impl_taut_no_left_lambda(P,[Q|Ps])--> agrees(P,[Q|Ps]).
impl_taut_no_left_lambda(Q,Ps)-->pred,pred,
  impl_taut_no_left_lambda((P->Q),Ps),
  impl_taut(P,Ps).  
 

implTaut(N,T):-impl_taut(N,T),natvars(T). 
  



tgo:-save_traning_set(6).

save_traning_set(M):-
  tell('training.txt'),
  encode_map(M),
  told.

encode_map(M):-
  do((
    between(0,M,N),
    encode_map1(N)
  )).

encode_map1(N):-do((
   tnf(N,X:T),
   numbervars(X,0,_),
   numbervars(T,0,_),
   encode_term(X,Xs,[]),
   encode_formula(T,Ts,[]),
   maplist(write,Ts),write(':'),maplist(write,Xs),nl
   )).
  
   
   
encode_term('$VAR'(I))-->['$VAR'(I)].
encode_term(l(X,E))-->[1],encode_term(X),encode_term(E).
encode_term(a(A,B))-->[0],encode_term(A),encode_term(B).

encode_formula(I)-->{integer(I)},!,['$VAR'(I)].
encode_formula('$VAR'(I))-->['$VAR'(I)].
encode_formula((A -> B))-->[0],encode_formula(A),encode_formula(B).


save_dataset(M):-
  do((
    between(0,M,N),
    save_dataset2(N)
  )).
  
  
save_dataset2(N):-
  make_directory_path('itaut/'),
  atomic_list_concat(['itaut/theorems',N,'.pro'],F),
  tell(F), 
  write('% clauses of the form: tp(Theorem,ProofTerm).'),nl,
  write('% preceeded by LaTeX code for Theorem and ProofTerm, as comments'),nl,nl,  
  do((
   tnf(N,X:T),
   write('% '),qqq(T),
   write('% '),qqq(X),
   portray_clause(tp(T,X))
  )),
  told.
  
  
  /*
  ?- findall(S,(between(0,16,N),sols(tnf(N,_),S)),Xs).
Xs = [0,1,2,3,7,17,43,129,389,1245,4274,14991,55289,210743,826136,3354509,13948176].

  
  */