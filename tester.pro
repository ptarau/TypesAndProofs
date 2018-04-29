% testing tools

:-dynamic(sure/2).

% nechmarks prover P on terms of size N

bm(N,P):-
  N1 is N//2,
  pbm(N,P,PT), % on all positive examples
  nbm(N1,P,NT), % on a blend, mostly negative examples
  T is PT+NT,
  writeln(time=[p=PT,n=NT,total=T]).

% tests for false positves or negatives
alltest(N,P):-N1 is N//2,
  ptest(N,P),
  ntest(N1,P).

% same, when lambda terms are generated as proof witnesses  
sprove1(T):-sprove1(T,_).

sprove1(T,X):-
  sprove(T,X0),%ppp(X0),
  ( cyclic_term(X0)-> (X=X0)
  ; evalLambdaTterm(X0,X)
  ),
  (X=@=X0->true;ppp(not_normal_form),ppp(X0),ppp(X)),
  true.

% tur a term in which variables are repsentede as Prolog vars  
varvars(A,X):-
  maxvar(A,L0),L is L0+1,
  functor(D,x,L),
  varvars(A,X,D).

varvars((A,B),(X->Y),D):-varvars(A,X,D),varvars(B,Y,D).
varvars(A->B,X->Y,D):-varvars(A,X,D),varvars(B,Y,D).
varvars(A,V,D):-integer(A),I is A+1,arg(I,D,V).
varvars(false,false,_).

% variable with larges index
maxvar(I,R):-integer(I),!,R=I.
maxvar(false,0):-!.
maxvar((A->B),R):-maxvar(A,I),maxvar(B,J),R is max(I,J).
maxvar((A,B),R):-maxvar(A,I),maxvar(B,J),R is max(I,J).

% turns a term int a ground one by banding
% logic variables in it to 0,1,...

natvars(T):-
  must_be(acyclic,T),
  term_variables(T,Vs),
  length(Vs,L1),
  L is L1-1,
  numlist(0,L,Vs).

% same, but throws in atom "false"
% as first variable to bind
classvars(T):-
  must_be(acyclic,T),
  term_variables(T,[false|Vs]),
  length(Vs,L1),
  L is L1-1,
  numlist(0,L,Vs).  
  
  
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

% just the terms, to testing, to help with
% bencharkin g exact time spent in proving
pbm0(N):-
  tnf(N,_:T),
  natvars(T),
  fail
; true.


% benchmark on type of normal forms of size N 
pbm(N,P,Time):-
  time(pbm0(N),T0),
  time(ptest(N,P),T1),
  Time is T1-T0.
  
pbm(N,P):-
  pbm(N,P,Time),
  writeln(time(pbm)=Time).

% test that a prover aggrees that the Glivenko not-not 
% transformation results in classical tautology
dneg_taut(P,T,NNT):-
  ( call(P,NNT) -> \+taut(T),
        ppp('success_but_not_a tautology!'(T))
  ; taut(T), ppp('fails_but_is_tautology!'(NNT))
  ).

% test al all classical formulas, via (a->false)->false  
ttest(N,P):-
  allClassFormulas(N,T,NNT),
  dneg_taut(P,T,NNT),
  fail
; true.
  
% random typed NF with fixed seed
genRanTyped:-genRanTyped(1003).


% generates random typed NFs with given seed
% Seed=random generates them pseudo-randomly

genRanTyped(Seed):-
  retractall(sure(_,_)),
  parRanTNF(Seed,50,70,1000,X:T,_),
  ppp(X),ppp(T),nl,
  assertz(sure(X,T)),
  fail
; tell('test_data/sure.pro'),listing(sure),told.
  
% test using surel well typed random terms in sure.pro
mrptest(P):-
  consult('test_data/sure.pro'),
  sure(_,T),
  natvars(T),
  call(P,T),
  ppp(T),
  fail
; true.

% sure terms turned from de Bruijn into
% canonical lambda terms  vith logic variables in them
lsure(X,T):-sure(X0,T),nb2l(X0,X).

nb2l(A,T):-nb2l(A,T,_Vs).

nat1(0).
nat1(s(_)).

% from de Bruijn to canonical lambda term
nb2l(I,V,Vs):-nat1(I),s2n(I,N),nth0(N,Vs,V).
nb2l(a(A,B),a(X,Y),Vs):-nb2l(A,X,Vs),nb2l(B,Y,Vs).
nb2l(l(A),l(V,Y),Vs):-nb2l(A,Y,[V|Vs]).

% run test using sprove/2 that returns inhabitant X of T
mrptest:-
  consult('test_data/sure.pro'),
  lsure(X0,T),
  natvars(T),
  
  ppp(X0),lsize(X0,L0),ppp(l0=L0),
  sprove(T,X),
  %ppp(T),
  %ppp(X),
  lsize(X,L),
  ppp([l0=L0,l=L]),
  nl,
  fail
; true.

% test generation of random NFs of size 20
test_ranNF:-ranNF(20,X:T,_),
   numbervars(X,10,_),numbervars(T,0,_),
   writeln(X),writeln(T),fail.
   
ranptest(N,P):-rptest(random,1,N,1,P).
  
rptest(N,P):-rptest(1001,20,N,100,P).

% test P against sure type T
rptest(Seed,TSize,N,K,P):-
  parRanTNF(Seed,TSize,N,K,X:T,Size),
  portray_clause(sure(X:T)),
  \+((natvars(T),call(P,T))),
  ppp('false negative, for term size'(Size)),
  ppp(while_term_inhabiting_it=X),
  ppp(no__inhabitant_found_for=T),nl,
  fail
; true.  
  
% banchark with binary 
% trees labeled in all possible ways  

nbm(N,P,Time):-
  time(nbm0(N),T0),
  time(nbm1(N,P),T1),
  Time is T1-T0.

nbm(N,P):-
  nbm(N,P,Time),
  writeln(time(nbm)=Time).

  nbm1(N,P):-
  call(allImpFormulas,N,T),
  call(P,T),
  fail
; true.

% basic generator for timing delta
nbm0(N):-call(allImpFormulas,N,_T), fail;true.

% test against Dyckhoff's prover as gold standard
cntest(N,P):-
  allClassFormulas(N,T),ppp(T),
  ( call(P,T) -> \+dprove(T),ppp(false_pos(T))
  ; dprove(T) -> dprove(T),ppp(false_neg(T))
  ), 
  fail
; true.

% test against sure tautologies
ntest(N,P):-ntest(N,allImpFormulas,P).

ntest(N,G,P):-
  call(G,N,T),
  call(P,T),
  must_be_taut(T),   
  fail
; true.


ntest_with(P,T):-ntest_with(5,true,P,T).

ntest_with(Trim,TestCorrectness,P,T):-
  %ppp(proving=T),
  ( call(P,T)->R=true
  ; R=false,0 is random(Trim)
  ),
  assert(proven(R,T)),
  (TestCorrectness->must_be_taut(T);true).
  
must_be_taut(T):-
  %intu=>taut
  %\+taut => \+intu
  \+taut(T),\+intu(T,_),
  ppp('false positive!!!'),
  ppp(proof_of_non_tautology_should_fail=T).

maybe_type(T):-
  ranImpFormulas(15,5,5,T).

    

% benchmark against large random terms
rnbm:-rnbm(dprove).

% replicable large random types
rnbm(P):-
  rnbm(101,500,2000,100,P,Time),
  ppp(Time).
 
rnbm(Seed,N,K,Trim,P,Time):-
  time(
    rntest(Trim,fail,Seed,N,K,P),
    Time
  ).

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

% type of S-combinator  
ts_(((A->(B->C))->((A->B)->(A->C)))).

apply_imp(P,Q,a(P,a(Q,l(Y,a(P,l(_,Y)))))).

% catches the case when a different inhabitant is found
% no big deal as there might be a lot of them
pstest(N,F):-
  tnf(N,X:T),copy_term(T,CT),
  %nl,ppp(X),ppp(T),
  natvars(CT),
  call(F,CT,CX),
  \+ (X=@=CX),
  ppp(while_term_inhabiting_it=X:T),
  ppp(other_inhabitant_found__=CX:CT),nl,
  fail.

  
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
  
% a few  quick tests

t1:-N=50,K=20,P=hprove,time(ran_typed(N,K,K,P,X:T)),ppp(X),ppp(T),fail.

t2:-L=5,N=5,K=50,time(ran_long_proof(N,L,K,X:T)),ppp(X),ppp(T),fail.

t3:-time(mrptest(pprove)).

t4:-ran_typed(25,5,5,pprove,R),ppp(R),fail.


t5:-allImpFormulas(2,T),ppp(T),fail.

t6:-rntest(10,42,10,5,pprove).

t7:-mrptest(pprove).

t8 :- ranptest(15,pprove).

t9:-alltest(2,badprove).

t10:-time(nstest(6,sprove)).

t11:-test_ranNF.

t12:-allImpFormulas(5,T),toHorn(T,H),toHorn(TT,H),ppp(H),ppp(T==TT),nl,fail.

htest:-hard(T),natvars(T),mprove(T).

ihard(H):-hard(H),natvars(H).

% false
hard(Term):-Term=(((A->B->C->((D->C)->E)->F)->_G)->H->C->E->(((((I->J)->((K->L)->M)->L)->(((C->F)->A)->N)->O)->P->N->Q)->R)->S->(D->T->U->(((((F->F->V->T)->Q->Q)->K->N)->E)->W)->J->((M->Q)->L->L)->P->_X->Q->D)->(E->((F->(((O->Q)->I)->H->F->N)->(((N->L->P->M)->(((A->D)->J->(Q->Y)->H)->F)->Z)->Z)->V)->M)->M->P)->(((K->V)->W)->K)->(C->I->S)->Z->L->U->((Q->R->(((_A1->B1->B)->W)->((D->W)->Y)->W)->B1)->((U->E)->U)->C->Y)->J).

% false
harder(ITerm):-
  ITerm=((0->1)->(((2->3)->4)->5)->(((6->((7->8)->9)->10)->11->12)->(((13->14)->0)->15)->((16->(17->(((18->19->20)->(21->((22->23)->24->8->((((25->26->((27->28->29->30)->31)->((((32->33)->(((34->((14->9)->35)->36)->37)->38)->((39->(((40->(((41->((42->24->17)->14)->25)->36)->43)->((((2->44)->45)->46)->(28->(35->(((8->38)->(47->48)->30)->27)->10->49)->50->(28->32->((((14->((51->52)->26)->14)->(53->54)->55)->56)->57)->36)->58)->(59->60->(44->(48->28)->61)->53)->62)->63->64)->65)->66->47)->(67->33)->68->(50->(22->((((27->69)->(63->42)->(((16->70->71->(48->9)->72)->73->25)->5)->(((39->74)->75)->76)->(62->(77->68)->(((((21->((74->46->8->78)->(6->79)->80)->6->81)->82->35)->(((((83->84->((38->(47->41)->38)->85->11)->86)->4->68)->87)->30)->((60->(49->4)->88)->89)->8)->65)->57)->(3->33->90->(44->7)->51->(((91->92->38->40)->6)->(((93->47)->2->23)->73)->93->94)->65)->87)->15->70)->75)->75)->(((((30->29->(51->26)->87->95)->48)->42)->96)->39->((54->4)->40)->3->24)->51)->12->25->66)->(((27->21)->31)->37)->8->52)->97)->(15->98)->64)->5)->76)->16->0)->(30->33)->4->(54->(0->94->99->((49->24)->88)->100)->55)->(((((((49->16)->83->12)->60->56)->101->70->(41->81)->52)->(76->27)->32)->74)->97->63)->38)->87)->59->102->(50->53)->29)->103->82)->((3->88->(93->46)->64->87)->104)->81->16->((102->77->(51->20->45)->26)->84->(((71->104)->50)->105)->106->28->20)->22->7)->92)->50->9)->45->(((30->(73->50)->107)->57)->41)->89)->27)->(42->14->12)->((23->14)->(85->(24->16)->28)->(((83->80->78)->47)->78->((94->45->(42->108)->16->0)->(80->(19->(77->64)->25->67)->23)->((88->55)->65)->63)->97)->(91->(22->16)->(((95->90->(((88->((20->73)->(18->100->9->43->106->30)->47)->85)->52->109)->(((21->108->78->0->9)->((9->22->22)->(69->27)->(36->(((42->65->(10->(86->98->(((69->87)->9->3)->68)->19->107->36)->76)->37->77)->1->95)->9)->(((5->11->94)->80)->92)->90)->101->54->74->59)->74)->97)->83)->(31->83)->42)->98->75)->98)->(((50->44)->25)->9)->(6->20)->(109->17)->54->26)->76)->38)->37->(((18->(21->(110->96->4)->79)->(40->41)->102)->41)->(((((84->((60->28)->(88->52->14->((21->96->(97->(((((75->16)->10->57)->108)->107)->((((101->33->((86->73->((68->77)->(41->41->(28->(104->80)->70)->96)->49->22->24)->67)->(39->85)->29->28)->(68->24)->(97->1)->46)->102)->(10->76->(29->51->94)->110)->110)->76)->36->12)->33)->20)->60)->((((5->11->47)->13->(45->69)->((27->105)->90->30)->60)->(83->47)->66->52)->25)->30)->78)->85)->13)->82->96->92)->27)->100)->15)->(66->94->78->23)->50)->71->101)->95->60->97->94->79->17).


makeSure:-makeSure(70,rrprove).

makeSure(N,P):-tell('test_data/sure.pro'),time(rptest(N,P)),told.

  /*
  benchmarks
  
  https://rd.host.cs.st-andrews.ac.uk/logic/marks.html
  https://rd.host.cs.st-andrews.ac.uk/logic/marks.html#TM
  
*/
  
  
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
  proven(_V,T),dneg(T,TT),dprove(TT),write(TT),write('.'),nl,fail.


  /*
  %%%   
zprove(T0):-toSortedHorn(T0,T),ljz(T,[]),!.

ljz(A,Vs):-memberchk(A,Vs),!. 
ljz((B:-[B]),_):-!.
%ljz((B:-As),_):-memberchk(B,As),!.
ljz((B:-As),Vs1):-!,add_all(As,Vs1,Vs2),ljz(B,Vs2).
ljz(G,Vs1):-zreduce(Vs1,Vs2),ljz(G,Vs2).

zreduce(Vs1,Vs4):-
  select((B:-As),Vs1,Vs2),
  select(A,As,Bs), 
  ljz_imp(A,B,D,Vs2,Vs3),
  ljz(D,Vs3),
  !,
  trimmed((B:-Bs),NewB),
  add_new(NewB,Vs3,Vs4).

ljz_imp(A,_B,A,Vs,Vs):-atomic(A),!.
ljz_imp((D:-Cs),B,D,Vs1,Vs2):-
  add_all([(B:-[D])|Cs],Vs1,Vs2).
  
  
  */
  
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
  %ppp((T-->TT)),
  ( call(Silver,T) -> \+ call(Gold,TT), R = wrong_success
  ; % \+ Silver
    call(Gold,TT) -> R = wrong_failure
  ; R = agreement
  ).
  
% shannot expansion - for (todo) use in fast classical
% tautology checking
shannon(V,T,T0,T1):-shannon0(V,T,T0),shannon1(V,T,T1).
  
  
shannon0(V,(V->_),true):-!.
shannon0(V,(X ->V),(NewX->false)):-!,shannon0(V,X,NewX).
shannon0(V,W,W):-atomic(W),W\=V,!.
shanoon0(V,(X->Y),(A->B)):-shannon0(V,X,A),shannon0(V,Y,B).

shannon1(V,(_ -> V),true):-!.
shannon1(V,(V -> X),NewX):-!,shannon1(V,X,NewX).
shannon1(V,W,W):-atomic(W),W\=V,!.
shannon1(V,(X->Y),(A->B)):-shannon1(V,X,A),shannon1(V,Y,B).



% tests "proven" formulas against Melvin Fitting's prover 
ftest2:-test_proven(tautology).

test_proven(P):-  
  proven(V,T),ppp(T=V),
  (call(P,T)->VV=true;VV=false),
  writeln(V==VV),
  fail
; true.

