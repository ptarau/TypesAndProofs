% various testing tools

:-dynamic(proven/2).

   
ranptest(N,P):-rptest(random,1,N,1,P).
  
rptest(N,P):-rptest(1001,20,N,100,P).

% test P against sure type T
rptest(Seed,TSize,N,K,P):-
  parRanTypedTNF(Seed,TSize,N,K,X:T,Size),
  portray_clause(sure(X:T)),
  \+(call(P,T)),
  ppp('false negative, for term size'(Size)),
  ppp(while_term_inhabiting_it=X),
  ppp(no__inhabitant_found_for=T),nl,
  fail
; true.  
  
% secial test data instances

  
% type of S-combinator  
ts_(((A->(B->C))->((A->B)->(A->C)))).

apply_imp(P,Q,a(P,a(Q,l(Y,a(P,l(_,Y)))))).


% a few  quick tests

t1:-N=50,K=20,P=hprove,time(ran_typed(N,K,K,P,X:T)),ppp(X),ppp(T),fail.

t2:-L=5,N=5,K=50,time(ran_long_proof(N,L,K,X:T)),ppp(X),ppp(T),fail.


t3:- tprove((a * b + b*c + c*d) -> (a+b+c )).

t4:-ran_typed(25,5,5,pprove,R),ppp(R),fail.


t5:-allImpFormulas(2,T),ppp(T),fail.

t6:-rntest(10,42,10,5,pprove).

t7:-allImpFormulas(2,T),
    abduce_for1(hprove,T,R),ppp((R-->T)),fail.

t8 :- ranptest(15,pprove).

t9:-alltest(2,badProve).

t10:-time(nstest(6,sprove)).



t11:-allImpFormulas(5,T),toHorn(T,H),toHorn(TT,H),ppp(H),ppp(T==TT),nl,fail.

t12:-parRanTypedTNF(42,50,50,10,XT,S),natvars(XT),ppp((size(S)=XT)),fail.


t13:- do hbm(8,ljh,T),ppp(T).

% K combinator
k_(0->1->0).
% S combinator
s_( (0->1->2)->(0->1)->(0->2)).
% Pierces's law
p_(((0 -> 1) -> 0) -> 0).
% derived from modus ponens
mp_(0->(0->1)->1).

axtest:-
  maplist(call,[k_,s_,mp_],Ts),
  maplist(bprove,Ts).


ptest:-p_(T),ppp(T),nl,abduce_imp(bprove,T,R),ppp(R),fail.

htest:-hard(T),natvars(T),hhprove(T).

ihard(H):-hard(H),natvars(H).

hhard(H):-ihard(I),toListHorn(I,H).



abfix(N,P):-
  allImpFormulas(N,T),
  \+ call(P,T),
  once(abduce_st(P,T,AbT)),
  ppp(AbT),
  fail.


% false
hard(Term):-Term=(((A->B->C->((D->C)->E)->F)->_G)->H->C->E->(((((I->J)->((K->L)->M)->L)->(((C->F)->A)->N)->O)->P->N->Q)->R)->S->(D->T->U->(((((F->F->V->T)->Q->Q)->K->N)->E)->W)->J->((M->Q)->L->L)->P->_X->Q->D)->(E->((F->(((O->Q)->I)->H->F->N)->(((N->L->P->M)->(((A->D)->J->(Q->Y)->H)->F)->Z)->Z)->V)->M)->M->P)->(((K->V)->W)->K)->(C->I->S)->Z->L->U->((Q->R->(((_A1->B1->B)->W)->((D->W)->Y)->W)->B1)->((U->E)->U)->C->Y)->J).

habtest:-ihard(H),abduce_st(hprove,H,T),ppp(H),nl,ppp(T),!,fail.

sound:-
  T0=(((C->D)->B)->G),
  T1=(C->(D->B)->D->G),
  T=(T0->T1),
  natvars(T),taut(T),
  pprove(T),ppp(one),
  TT=(T1->T0),
  taut0(TT),ppp(two),  
  pprove(TT),ppp(three).
  

clastest:-
  proven(_V,T),dneg(T,TT),
  dprove(TT),
  write(TT),
  write('.'),nl,fail.

  
cotest1(N):-cotest(N,(=),dprove,pprove).
  
cotest2(N):-cotest(N,(=),pprove,zprove).

cotest3(N):-cotest(N,(=),intu,zprove).

cotest4(N):-cotest(N,(dneg),cprove,tautology).

cotest(N,Transformer,Gold,Silver):-
  allImpFormulas(N,T),
  cotest_one(Transformer,Gold,Silver,T, R),
  R\=agreement,
  ppp(T=R),
  fail
  ; true.
  
cotest_one(Transformer,Gold,Silver,T, R):-
  call(Transformer,T,TT),
  ( call(Silver,T) -> \+ call(Gold,TT), R = wrong_success
  ; % \+ Silver
    call(Gold,TT) -> R = wrong_failure
  ; R = agreement
  ).
  

gotest1(N):-do((
  gold_test(N,bprove,Culprit,Unexpected),
  ppp([Culprit,Unexpected])
)).
  
gotest2(N):-do((
  gold_classical_test(N,gprove,Culprit,Unexpected),
  ppp([Culprit,Unexpected])
  )).

gold_test(N,Silver,Culprit,Unexpected):-
  gold_test(N,allImpFormulas,(=),dprove,Silver, Culprit,Unexpected).

gold_test(N,Generator,Transformer,Gold,Silver, Term0, Res):-
  call(Transformer,Term0,Term),
  call(Generator,N,Term0),
  gold_test_one(Gold,Silver,Term, Res),
  Res\=agreement.
  
gold_test_one(Gold,Silver,T, Res):-
  ( call(Silver,T) -> \+ call(Gold,T), 
    Res = wrong_success
  ; call(Gold,T) -> % \+ Silver
    Res = wrong_failure
  ; Res = agreement
  ).
  
  
gold_classical_test(N,Silver,Culprit,Unexpected):-
  gold_test(N,allClassFormulas,(=),tautology,Silver, Culprit,Unexpected).
 
 
 
gold_ran_imp_test(N,K, Silver, Culprit, Unexpected):-
  gold_test(N,genRanImpFormulas(K),(=),dprove,Silver, Culprit, Unexpected).  

rtest1:-
 gold_ran_imp_test(50,100,hprove, Culprit, Unexpected),ppp(Culprit=Unexpected).
  
rtest2:-time(
 gold_ran_imp_test(1000,1000,hprove, Culprit, Unexpected),ppp(Culprit=Unexpected)
).

% tests "proven" formulas against Melvin Fitting's prover 
ftest2:-test_proven(tautology).

test_proven(P):-  
  proven(V,T),ppp(T=V),
  (call(P,T)->VV=true;VV=false),
  writeln(V==VV),
  fail
; true.

biglamb:-
  ranImpFormulas(42,1000,1000,T),hprove(T),sprove(T,X),
  type_of(X,TT),
  ppp(X),
  ppp(T),
  ppp(TT),
  nl,
  fail
; ppp(done).

  
  