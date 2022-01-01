% various testing tools

:-dynamic(proven/2).
 
max_time(16).

% adaptor to run ILPT benchmarks from http://www.iltp.de
/*
?- fbm(4,faprove).
counts=(22026/222449=0.0990159542187198)
time=(0.8859989643096924=1.1487998962402344-0.262800931930542)
true.

?- fbm(4,faprove).
counts=(22026/222449=0.0990159542187198)
time=(0.8774759769439697=1.1507987976074219-0.27332282066345215)
true.

?- fbm(5,faprove).
counts=(898708/10548057=0.08520128399002774)
time=(64.96450614929199=76.97363495826721-12.00912880897522)

?- fbm(6,faprove).
counts=(42998147/579007337=0.07426183444027756)
time=(4630.17018699646=5346.297565937042-716.1273789405823)
true.

?- fbm(5,dprove).
counts=(898708/10548057=0.08520128399002774)
time=(66.07874488830566=77.83711385726929-11.758368968963623)

?- fbm(6,dprove).
counts=(42998147/579007337=0.07426183444027756)
time=(4996.20118522644=5652.213847160339-656.0126619338989)
*/


% not many purely implicational formulas at ILTP.org :-(

%[prover=bprove,total=274,skipped=249,tried=25:[right=25:[proven=23,refuted=2],wrong=0,timed_out(secs,6)=0,error=0]]
test_probs0:-time(test_probs(i_filter,bprove)).



% [prover=faprove,total=274,skipped=0,tried=274:[right=156:[proven=97,refuted=59],wrong=0,timed_out(secs,6)=118,error=0]]

% [prover=faprove,total=274,skipped=0,tried=274:[right=161:[proven=99,refuted=62],wrong=0,timed_out(secs,60)=113,error=0]]
% 44,126,013,525 inferences, 6873.586 CPU in 6881.504 seconds (100% CPU, 6419650 Lips)
% 44,126,013,639 inferences, 6873.586 CPU in 6881.504 seconds (100% CPU, 6419650 Lips)

test_probs1:-time(test_probs(faprove)).




% [prover=fpprove,total=274,skipped=0,tried=274:[right=150:[proven=96,refuted=54],wrong=0,timed_out(secs,6)=124,error=0]]
%[prover=fpprove,total=274,skipped=0,tried=273:[right=158:[proven=99,refuted=59],wrong=0,timed_out(secs,300)=115,error=1]]
% with andPar only
test_probs1p:-test_probs(fpprove).

%[prover=fxprove,total=274,skipped=0,tried=274:[right=157:[proven=97,refuted=60],wrong=0,timed_out(secs,16)=117,error=0]]
% 12,598,768,252 inferences, 1904.692 CPU in 1909.755 seconds (100% CPU, 6614597 Lips)
test_probs1x:-test_probs(fxprove).

% expanded to long list of shallow expressions in antecedent
test_probs1fl:-test_probs(flprove).

% same, but with simplified prover, knowing whay to reduce
test_probs1fr:-test_probs(flprove).

%[prover=par_faprove,total=274,skipped=0,tried=274:[right=155:[proven=98,refuted=57],wrong=0,timed_out(secs,6)=119,error=0]]
% scrambled
test_probs2p:-time(test_probs(par_faprove)).


test_probs1c:-time(test_probs(fcprove)).

%[prover=ffprove,total=274,skipped=0,tried=274:[right=152:[proven=97,refuted=55],wrong=0,timed_out(secs,3)=122,error=0]]
test_probs2:-time(test_probs(ffprove)).

% [prover=fftprove,total=274,right=150:[proven=95],refuted=55,wrong=0,timed_out(secs,16)=124,error=0]
test_probs2a:-time(test_probs(fftprove)).

% [prover=dprove,total=274,right=171:[proven=108],refuted=63,wrong=0,timed_out(secs,16)=52,error=51]
% 32GB stacks
% [prover=dprove,total=274,skipped=0,tried=274:[right=175:[proven=109,refuted=66],wrong=0,timed_out(secs,60)=99,error=0]]
% 32GB [prover=dprove,total=274,skipped=0,tried=232:[right=180:[proven=111,refuted=69],wrong=0,timed_out(secs,600)=52,error=42]]
% 64GB stacks [prover=dprove,total=274,skipped=0,tried=271:[right=181:[proven=111,refuted=70],wrong=0,timed_out(secs,1500)=90,error=3]]
% 96GB [prover=dprove,total=274,skipped=0,tried=236:[right=181:[proven=111,refuted=70],wrong=0,timed_out(secs,600)=55,error=38]]

test_probs3:-time(test_probs(dprove)).
test_probs3p:-time(test_probs(par_dprove)).

% tester for g4prove
% [total=274,right=164,wrong=52,timed_out(secs,6)=43,error=15]
test_probs4:-time(test_probs(g4prove)).

% tester for ileantap
%[prover=ilprove,total=274,right=34,wrong=0,timed_out(secs,6)=219,error=21]
%[prover=ilprove,total=274,right=34,wrong=0,timed_out(secs,16)=219,error=21]
%32GB [prover=ilprove,total=274,skipped=0,tried=271:[right=35:[proven=31,refuted=4],wrong=0,timed_out(secs,60)=236,error=3]]
test_probs5:-time(test_probs(ilprove)).

%[prover=sep_prove,total=274,skipped=0,tried=274:[right=50:[proven=46,refuted=4],wrong=0,timed_out(secs,60)=224,error=0]]
test_probs6:-time(test_probs(sep_prove)).

test_probs7:-time(test_probs(coprove)).

% restricted to ->, <->
% [prover=fbprove,total=274,skipped=226,right=34:[proven=30],refuted=4,wrong=0,timed_out(secs,3)=14,error=0]
test_probs8:-time(test_probs(fb_filter,fbprove)).


% restricted to ->, <->
% [prover=haprove,total=274,skipped=226,right=33:[proven=30],refuted=3,wrong=0,timed_out(secs,6)=15,error=0]
test_probs9:-time(test_probs(fb_filter,haprove)).


% random, just for testing the tester
test_probs10:-time(test_probs(badProve)).

%[prover=ichprove,total=274,skipped=119,tried=155:[right=86:[proven=42,refuted=44],wrong=0,timed_out(secs,6)=69,error=0]]
% [prover=ichprove,total=274,skipped=119,tried=155:[right=92:[proven=44,refuted=48],wrong=0,timed_out(secs,60)=63,error=0]]
% 18,312,812,596 inferences, 4007.917 CPU in 4015.069 seconds (100% CPU, 4569159 Lips)
test_probs11:-time(test_probs(nest_filter,ichprove)).


test_probs11a:-time(test_probs(nest_filter,hmprove)).


% for camparison with nested Horn
% [prover=faprove,total=274,skipped=119,tried=155:[right=99:[proven=61,refuted=38],wrong=0,timed_out(secs,6)=56,error=0]]
% 1,275,469,297 inferences, 341.826 CPU in 342.158 seconds (100% CPU, 3731341 Lips)
% [prover=faprove,total=274,skipped=119,tried=155:[right=102:[proven=62,refuted=40],wrong=0,timed_out(secs,60)=53,error=0]]
% 11,839,580,607 inferences, 3227.189 CPU in 3230.193 seconds (100% CPU, 3668697 Lips)
test_probs12:-time(test_probs(nest_filter,faprove)).

test_probs13:-time(test_probs(fmprove)).

% [prover=fredprove,total=274,skipped=119,tried=155:[right=48:[proven=38,refuted=10],wrong=0,timed_out(secs,6)=107,error=0]]
% [prover=fredprove,total=274,skipped=119,tried=155:[right=49:[proven=39,refuted=10],wrong=0,timed_out(secs,16)=106,error=0]]
% [prover=fredprove,total=274,skipped=119,tried=155:[right=57:[proven=45,refuted=12],wrong=0,timed_out(secs,300)=98,error=0]]
test_probs14:-time(test_probs(nest_filter,fredprove)).

test_probs15:-time(test_probs(nest_filter,gredprove)).

% [prover=xfprove,total=274,skipped=0,tried=274:[right=149:[proven=93,refuted=56],wrong=0,timed_out(secs,6)=125,error=0]]
test_probs16:-time(test_probs(xfprove)).


test_probs17:-time(test_probs(hand_filter,handprove)).

% [prover=ljxprove,total=274,skipped=119,tried=155:[right=87:[proven=42,refuted=45],wrong=0,timed_out(secs,6)=68,error=0]]
% 5,043,398,671 inferences, 430.363 CPU in 431.955 seconds (100% CPU, 11718943 Lips)
test_probs18:-time(test_probs(hand_filter,ljxprove)).

% BEST 3rd party

% [prover=fcube,total=274,skipped=0,tried=274:[right=260:[proven=125,refuted=135],wrong=0,timed_out(secs,6)=14,error=0]]
% 1,213,923,468 inferences, 105.541 CPU in 105.704 seconds (100% CPU, 11501926 Lips)
% 1,213,923,582 inferences, 105.541 CPU in 105.705 seconds (100% CPU, 11501917 Lips)
%[prover=fcube,total=274,skipped=0,tried=274:[right=261:[proven=126,refuted=135],wrong=0,timed_out(secs,16)=13,error=0]]
% 2,781,180,708 inferences, 238.631 CPU in 238.948 seconds (100% CPU, 11654711 Lips)
% 2,781,180,822 inferences, 238.632 CPU in 238.948 seconds (100% CPU, 11654707 Lips)
% [prover=fc,total=274,skipped=0,tried=274:[right=262:[proven=127,refuted=135],wrong=0,timed_out(secs,600)=12,error=0]]
% 86,111,898,153 inferences, 7310.606 CPU in 7321.506 seconds (100% CPU, 11779037 Lips)
% 86,111,898,267 inferences, 7310.606 CPU in 7321.506 seconds (100% CPU, 11779037 Lips)
/*
42:probs/SYJ/SYJ202+1.009.pl=is(timeout(600))+should_be(true)
........
53:probs/SYJ/SYJ202+1.020.pl=is(timeout(600))+should_be(false)

% same with version 11
[prover=fcube,total=274,skipped=0,tried=274:[right=261:[proven=126,refuted=135],wrong=0,timed_out(secs,16)=13,error=0]]
*/
test_probs19:-time(test_probs(fcube)).


% test_probs20:-time(test_probs(bdprove)).

% -----------------------

bug17:-T=((0&1<->0)<->(0->1)),
  handprove(T).
  
bug17a:-T=(~ (p->q)->q->p),handprove(T).

bug17b:-
 T=(s->(~ (t->r)->p)-> ~ ((p->q)&(t->r))-> ~ ~p&s&s),
 handprove(T).

 
gotest1(N):-do((
  gold_test(N,bprove,Culprit,Unexpected),
  ppp([Culprit,Unexpected])
)).
  
gotest2(N):-do((
  gold_classical_test(N,gprove,Culprit,Unexpected),
  ppp([Culprit,Unexpected])
  )).

% impl + equiv testers
  
gotest3(N):-do((
  gold_eq_test(N,fbprove,Culprit,Unexpected),
  ppp([Culprit,Unexpected])
  )).  

gotest4(N):-do((
  gold_eq_test(N,haprove,Culprit,Unexpected),
  ppp([Culprit,Unexpected])
  )).    

gotest5(N):-do((
  gold_eq_test(N,hqprove,Culprit,Unexpected),
  ppp([Culprit,Unexpected])
  )).      


fulltest1(N):-do((
  gold_full_test(N,faprove,Culprit,Unexpected),
  ppp([Culprit,Unexpected])
  )).   
  
fulltest2(N):-do((
  gold_full_test(N,fcprove,Culprit,Unexpected),
  ppp([Culprit,Unexpected])
  )).   
  
fulltest3(N):-do((
  gold_full_test(N,xfprove,Culprit,Unexpected),
  ppp([Culprit,Unexpected])
  )).
  
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


t13:- do hbm(8,ljh,Counts,T),ppp(T),K is Counts,ppp(K).


t14:- do((
   T=(((a->b)->c) -> (a->(b->c))), 
   ppp(t=T),
   sprove(T,X),
   ppp(x=X),
   type_of(X,TT),
   ppp(tt=TT),
   natvars(TT),
   sprove(TT,XX),
   ppp(xx=XX)
)).
   
t15:-
 A=(1->2->(5->((7->8->9)->6)->4)->0),
 toHorn(A,H),
 flattenHorn(9,H,F),
 ppp(A),
 ppt(A),
 nl,
 
 ppp(H),
 ppt(H),
 hdepth(H,DH),
 ppp(hdepth=DH),
 nl,
 
 ppp(F),
 ppt(F),
 nl,
 hdepth(F,DF),
 ppp(hdepth=DF).


t16:-toEqHorn((a->(b<->c)->((d->a)<->(e->f->g))),R),ppp(R).

t17:-T=(((a<->b)->c)<->((b<->a)->c)),ppp(T),nl,toEqHorn(T,H),ppp(H).

t18:-T=(((a<->b)->c)<->((b<->a)->c)),ppp(T),nl,
  haprove(T).

t19:-lj3(T),ppp(T=valid),sprove(T,L),ppp(L),fail.

t20:-toNestedHorn((a<->(b<->c))<->((b<->c)<->a),R),ppp(R),fail.

t21:- hmints((a<->(b<->c))<->((b<->c)<->a),R),ppp(R),fail.

lj3(((a->b)->a)->(b->g) -> ((a->b)->g)).
lj3i(((0->1)->0)->(1->2) -> ((0->1)->2)).

% K combinator
k_(0->1->0).
% S combinator
s_( (0->1->2)->(0->1)->(0->2)).
% Pierces's law
p_(((0 -> 1) -> 0) -> 0).
% derived from modus ponens
mp_(0->(0->1)->1).

% S,K,X-combinators and their types
% Rosser's X-combinator: \f.fKSK

xC(l(A, a(a(a(A, l(B, l(_C, B))), l(D, l(E, l(F, a(a(D, F), a(E, F)))))), l(G, l(_H, G))))).

xK(K):-xC(X1),xC(X2),xC(X3),X12=a(X1,X2),K=a(X12,X3).
xS(S):-xC(X1),xC(X2),xC(X3),X23=a(X2,X3),S=a(X1,X23).

xT(T):-xC(X),type_of(X,T),natvars(T).
kT(T):-xK(X),type_of(X,T),natvars(T).
sT(T):-xS(X),type_of(X,T),natvars(T).

xcombtest:-
  xT(T),sprove(T,X),type_of(X,TT),natvars(TT),ppp(T),ppp(TT),fail.

axtest:-
  maplist(call,[k_,s_,mp_],Ts),
  maplist(bprove,Ts).

ptest:-p_(T),ppp(T),nl,abduce_imp(bprove,T,R),ppp(R),fail.

htest:-hard(T),natvars(T),hprove(T).

ihard(H):-hard(H),natvars(H).

hhard(H):-ihard(I),toListHorn(I,H).

fails1(X):-X=((a->b&c&d)<->((a&(b&c&d->g))->g)).
fails1:- fails1(X),faprove(X).

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
 
gold_nested_test(N,Prover,Culprit,Unexpected):-
   %Prover=ichprove,
   gold_test(N,allNestedFormulas,(=),dprove,Prover, Culprit,Unexpected).
  
gold_imp_test(N,Prover,Culprit,Unexpected):-
  gold_test(N,allImpFormulas,(=),dprove,Prover, Culprit,Unexpected).   
   
gold_eq_test(N,Prover,Culprit,Unexpected):-
  gold_test(N,allEqImplFormulas,(=),dprove,Prover, Culprit,Unexpected).
  
gold_eq_neg_test(N,Prover,Culprit,Unexpected):-
gold_test(N,allEqNegFormulas,(=),faprove,Prover, Culprit,Unexpected).

gold_classical_test(N,Silver,Culprit,Unexpected):-
  gold_test(N,allClassFormulas,(=),tautology,Silver, Culprit,Unexpected).
 
gold_full_test(N,Culprit,Unexpected):-
  gold_full_test(N,faprove,Culprit,Unexpected).
    
  
gold_full_test(N,Prover,Culprit,Unexpected):-
  gold_test(N,allFullFormulas,(=),dprove,Prover, Culprit,Unexpected).

gold_ext_test(N,Prover,Culprit,Unexpected):-
  gold_test(N,allExtFullFormulas,(=),dprove,Prover, Culprit,Unexpected).

% hardened tests

gold_hard_full_test(N,Prover,Culprit,Unexpected):-
  gold_test(N,allFullFormulas,(=),dprove,Prover, Culprit,Unexpected).
 
gold_DB_full_test(N,Prover,Culprit,Unexpected):-
  gold_test(N,allFullDBFormulas,(=),dprove,Prover, Culprit,Unexpected).

  
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
  ranImpFormulas(42,1000,1000,T),
  hprove(T),sprove(T,X),
  type_of(X,TT),
  ppp(X),
  ppp(T),
  ppp(TT),
  nl,
  fail
; ppp(done).

% sprove?
nobug:- 
   ranTNF(42,50,X:T),
   ppp(x=X),
   ppp(t=T),
   type_of(X,T0),
   subsumes_term(T0,T),
   ppp(t0=T0),
   T =T0,
   sprove(T,XX),
   ppp(xx=XX),
   type_of(XX,TT),
   ppp(TT).

nobug0:-
   T=(0->(((1->2)->3)->2)->((1->2)->3)->3),
   sprove(T,X),   
   hprove(T),
   ppp(t=T),
   ppp(x=X),
   %ppt(X),
   type_of(X,TT),
   ppp(tt=TT),
   ppp(done).
   
tamari:-
  sprove(((0->1)->2) -> (0->(1->2))).
  

tam(T):-T=(((0->1)->2) -> (0->(1->2))).

tamari1:-
  T=(((0->1)->2) -> (0->(1->2))),
  ppp(T),
  ljs(X,T,[]),
  ppp(X),
  type_of(X,TT),natvars(TT),
  ppp(TT),
  ljs(XX,TT,[]),
  ppp(XX),
  type_of(XX,TTT),natvars(TTT),
  ppp(TTT),
  fail.
  
tamari2:-
  T=(((A->B)->C) -> (A->(B->C))),
  ppp(t_=T),natvars(T),
  ljs(X,T,[]), 
  type_of(X,TT),
  ppp(tt=TT),
  ppp(X),
  T=TT, % not unifiable types
  ppp(eq=T).
  

satform0((p->b) -> ((x->b)->c) ->(p->c)).


satform((p->q->a->b) -> ((a->b)->c) ->(p->q->c)).

satform1((p->q->c)->(p->q->a->b) -> ((a->b)->c)).

  
  
:-dynamic(fof/3).



% a graph coloring problem in Prolog
 
e(r,g).
e(r,b).
e(b,g).
e(b,r).
e(g,r).
e(g,b).

ggraph([C1,C2,C3,C4,C5,C6]):-
   e(C1,C2),e(C2,C3),e(C1,C3),e(C3,C4),e(C4,C5),
   e(C5,C6),e(C4,C6),e(C2,C5),e(C1,C6).
  
% same, by proving an intuitionistic formula 
cmap([C1,C2,C3,C4,C5,C6],[r,g,b],
    
  (r-g) &
  (r-b) &
  (b-g) & 
  (g-r) &
  (b-r) &
  (g-b) 
  ->
  (C1-C2) &
  (C2-C3) &
  (C1-C3) &
  (C3-C4) &
  (C2-C5) &
  (C4-C5) &
  (C5-C6) &
  (C4-C6) &
  (C1-C6) 
  
  ).

colors(Vs,Gs):-
  cmap(Vs,Cs,Gs),
  maplist(element_of(Cs),Vs).

element_of(Vs,V):-member(V,Vs).
   
coltest(Vs+Formula):-
  colors(Vs,Formula),
  ichprove(Formula).
 
coltest1(Vs+Formula):-
  cmap(Vs,_,Formula),
  xljf(Formula).
  
gcoltest(Vs+Formula):-
  cmap(Vs,_,Formula),
  nhornprove(Formula).
  
fcoltest(Vs+Formula):-
  colors(Vs,Formula),
  fmprove(Formula).  
  
mcoltest(Vs+Formula):-
  colors(Vs,Formula),
  faprove(Formula).  
  
% simple graph coloring in Prolog

qcolor(Es):-
  Es =
  e(C1,C2) ,
  e(C2,C3) ,
  e(C1,C3) ,
  e(C3,C4) ,
  e(C2,C5) ,
  e(C4,C5) ,
  e(C5,C6) ,
  e(C4,C6) ,
  e(C1,C6),
  Cs=[r,g,b],
  qcolor(Es,Cs).

qcolor(Es,Cs):-maplist(edge(Cs),Es).

edge(Cs,e(X,Y)):-select(X,Cs,Ds),member(Y,Ds).

% end


satprob(P,[X,Y,Z,U]):-P=(
  f(_,0) & t(_,1)
  ->
  (
    t(x,X) & f(y,Y) & t(z,Z) v
    f(x,X) & t(y,Y) & t(u,U) v
    t(z,Z) & f(u,U)
  )
)
.
  

gsatprob(P,Vs):-
  satprob(P,Vs),
  maplist(ground01,Vs).
  
ground01(0).
ground01(1).
  

sx_(((v0(V0)->v1(V1)->v2(V2))->(v0(V0)->v1(V1))->v0(V0)->v2(V2))). 


eq1:-T=((a->b)<->a)<->(a&b),faprove(T).

eq2:-A=0<->(0<->(1<->0)),frprove(A).

eq3:-faprove(((p->q)->r)->(q->r)).

eq4:-
  T1=(c<->(a->b)),
  X=(~a v b v ~c),Y=(a v c),Z=(~b v c),
  T2=(X & Y & Z),
  E=(T1<->T2),
  ppp(E),
  tautology(E),
  ppp(taut),
  \+faprove(E),
  ppp(not_intu).
  

fbug:-T=(((0 <-> (((0 <-> 0) <-> 0) -> false)) -> false) -> false) ,
  ppp(T),
  faprove(T).
  
% soundness of ljt4 rule  
ljt4:-L=((d->b)->(c->d)),R=(b->g),LR=(((c->d)->b)->g),
   bprove(L->(R->LR)).
   
   
/*
assume 
  ((d->b)->(c->d))
  ?(b->g)->(((c->d)->b)->g)
  
assumed
  ((d->b)->(c->d)), b->g
  ? (((c->d)->b)->g)
   
  assumed ((c->d)->b) , b->g, ((d->b)->(c->d)), 
  ?g
  
  (d->b)->b, b->g
*/

ljt4a:-ljb(g, [
  ((d->b)->(c->d)), ((c->d)->b) , b->g
]).

ljt4b:-ljb(b, [
  ((d->b)->(c->d)), ((c->d)->b) 
  ]).

ljt4bb:-X=(c->d),ljb(b, [
  ((d->b)->X), (X->b) 
  ]).

ljt4c:-ljb(b, [ % QED
  (d->b),b 
  ]).  
  
  
  
hrbug:-
  hrprove((0:-[(0:-[0])])). 
 
nestest:-nestest(5). 
nestest(N):-gold_nested_test(N,ichprove,C,U),ppp(C:U),fail.

handtest:-nestest(5). 
handtest(N):-gold_nested_test(N,handprove,C,U),ppp(C:U),fail.

ljxtest:-nestest(5). 
ljxtest(N):-gold_nested_test(N,ljxprove,C,U),ppp(C:U),fail.

% should fail as intu and class tauts are the same
classimp0(M,A):-
  between(0,M,N),
  allImpFormulas0(N,A),
  tautology(A),
  \+ eprove(A).

  
classimp0:-
 \+ classimp0(12,_).

 
 
 iltp(Prover):-
    consult('test_data/iltp.pro'),
    do((
      iltp(N,A,B,C),ppp(N:A+B),
      catch(time(call(Prover,C)),E,(ppp(E),sleep(20)))
    )).

