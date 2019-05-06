:-dynamic(sure/2).


% tests for false positves or negatives
alltest(N,P):-N1 is N//2,
  ptest(N,P),
  ntest(N1,P).

  
  
% false negative only test
ptest0(N,P):-ptest(fail,N,P).

% catches terms on wich P succeeds
% that are not theorems
% by testing against the types of all normal forms
% up to size N
ptest(N,P):-ptest(true,N,P).

ptest(NoVars,N,P):-
  tnf(N,X:T),
  (NoVars->natvars(T);true),
  %nl,ppp(X),ppp(T),
  \+call(P,T),
  ppp('false negative!!!'),
  ppp(while_term_inhabiting_it=X),
  ppp(no__inhabitant_found_for=T),nl,
  fail
; true.


% test that a prover aggrees that the Glivenko's not-not 
% transformation results in classical tautology
dneg_taut(P,T,NNT):-
  ( call(P,NNT) -> \+taut(T),
        ppp('success_but_not_a tautology!'(T))
  ; taut(T), ppp('fails_but_is_tautology!'(NNT))
  ).

% test al all classical formulas, via (a->false)->false 
% e.g ?- ttest(6,cprove).
ttest(N,P):-
  allClassFormulas(N,T,NNT),
  dneg_taut(P,T,NNT),
  fail
; true.

% catches the case when a different inhabitant is found
% no big deal as there might be a lot of them
pstest(N,F):-
  tnf(N,X:T),copy_term(T,CT),
  %nl,ppp(X),ppp(T),
  natvars(CT),
  call(F,CT,CX),
  ( \+type_of(CX,_)->
    ppp(sprove(CT,is_bad)),
    ppp(untypable_lambda_term=CX),nl,
    assertion(type_of(CX,_))
  ;  
    \+ (X=@=CX)
   % ,ppp(while_term_inhabiting_it=X:T),
   % ppp(other_inhabitant_found__=CX:CT),nl
  ),
  fail.

  % same, when lambda terms are generated as proof witnesses  
sprove1(T):-sprove1(T,_).

sprove1(T,X):-
  sprove(T,X0),%ppp(X0),
  assertion(acyclic_term(X0)),
  evalLambdaTterm(X0,X),
  (X=@=X0->true;ppp(not_normal_form),ppp(X0),ppp(X)),
  true.
  
  
% test against all well formed implicational expressions

ntest(N,P):-ntest(N,allImpFormulas,P).

ntest(N,G,P):-
  new_ctr(All),new_ctr(Yes),
  do((
    call(G,N,T),ctr_inc(All),
    call(P,T),ctr_inc(Yes),
    must_be_taut(T)
  )),
  ctr_get(All,A),
  ctr_get(Yes,Y),
  U is A-Y,
  ppp([total,A,provable,Y,unprovable,U]).
  

must_be_taut(T):-
  tautology(T),
  dprove(T),
  % intu(T), too slow
  !.
must_be_taut(T):-ppp(succeeded_but_not_a_tautology(T)),fail.

ntest_with(P,T):-ntest_with(5,true,P,T).

ntest_with(Trim,TestCorrectness,P,T):-
  %ppp(proving=T),
  ( call(P,T)->R=true
  ; R=false,0 is random(Trim)
  ),
  assert(proven(R,T)),
  (TestCorrectness->must_be_taut(T);true).
  
ntest_with(T):-
  GoldIntu=dprove,
  %GoldIntu=intu,
  GoldClass=tautology,
  %GoldClass=taut,
  %intu=>taut
  %\+taut => \+intu
  ( \+call(GoldClass,T)-> ppp('not a classical tautology!!!')
  ; \+ call(GoldIntu,T)->ppp('not an intuitionistic tautology!!!')
  ; fail
  ),
  ppp('false positive!!!'),
  ppp(proof_of_non_tautology_should_fail=T),
  ppp('------------').

% catches if inahbitants type is unexpected
rnstest(N,F):-
  G=ranImpFormula,
  call(G,N,F).
  
nstest(N,F):-
  G=allImpFormulas,
  nstest(N,G,F).

nstest(N,G,F):-
  call(G,N,T),
  % ppp(T),
  once((
  copy_term(T,CT),
    call(F,CT,X),
    (cyclic_term(X)->ppp(cyclic=X),fail;true),
    % ppp(x=X),
    type_of(X,T0)
    %,ppp(t=T0)
  )),
  % passing this means a problem
  \+((subsumes_term(T0,T)
     ;
     subsumes_term(T,T0)
  )),
  ppp('inhabitant has wrong type'),   
  ppp(expected_type=T),
  ppp(computed_type=T0),
  intu(12,T,GoodX),
  ppp(good_term=GoodX),
  ppp(bad__term=X),
  ppp('----').

with(P,X):-
  varvars(X,T),
  call(P,T).

  
% test prover P aginst K random instance of size N
rntest(N,K,P):-rntest(10,random,N,K,P).
  
rntest(Trim,Seed,N,K,P):-rntest(Trim,true,Seed,N,K,P).

rntest(Trim,Corr,Seed,N,K,P):-
   retractall(proven(_,_)),
   ( ranImpFormulas(Seed,N,K,T),
     %ppp(T),
     ntest_with(Trim,Corr,P,T),
     fail
  ;  true
  ).
  
% call memo_maybe(N,K) first  
rntest_maybe(P):-
   ( maybe_type(T),
     ntest_with(P,T),
     fail
  ;  true
  ).

  
bigrlamb(T):-
  T=((_A->_B->((C->D->D)->E->F->G)->(((E->F->G)->G)->
    ((E->F->G)->G)->C->D->D)->
    ((E->F->G)->G)->E->F->G)),
  natvars(T).