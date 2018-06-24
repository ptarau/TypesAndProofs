% various testing tools

:-dynamic(proven/2).
 
max_time(3).

% adaptor to run ILPT benchmarks from http://www.iltp.de/  

% [prover=faprove,total=274,right=154:[proven=98],refuted=56,wrong=0,timed_out(secs,16)=120,error=0]
load_probs1:-time(load_probs(faprove)).



load_probs1c:-time(load_probs(fcprove)).


% [prover=ffprove,total=274,right=150:[proven=95],refuted=55,wrong=0,timed_out(secs,16)=124,error=0]
load_probs2:-time(load_probs(ffprove)).



% [prover=dprove,total=274,right=171:[proven=108],refuted=63,wrong=0,timed_out(secs,16)=52,error=51]
load_probs3:-time(load_probs(dprove)).

% tester for g4prove
% [total=274,right=164,wrong=52,timed_out(secs,6)=43,error=15]
load_probs4:-time(load_probs(g4prove)).

% tester for ileantap
%[prover=ilprove,total=274,right=34,wrong=0,timed_out(secs,6)=219,error=21]
%[prover=ilprove,total=274,right=34,wrong=0,timed_out(secs,16)=219,error=21]

load_probs5:-time(load_probs(ilprove)).


load_probs6:-rime(load_probs(coprove)).

% restricted to ->, <->
% [prover=fbprove,total=274,skipped=226,right=34:[proven=30],refuted=4,wrong=0,timed_out(secs,3)=14,error=0]
load_probs7:-time(load_probs(fb_filter,fbprove)).


% restricted to ->, <->
% [prover=haprove,total=274,skipped=226,right=33:[proven=30],refuted=3,wrong=0,timed_out(secs,6)=15,error=0]

load_probs8:-time(load_probs(fb_filter,haprove)).


% random, just for testing the tester
load_probs9:-time(load_probs(badProve)).


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

htest:-hard(T),natvars(T),hprove(T).

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
  
  
gold_eq_test(N,Prover,Culprit,Unexpected):-
  gold_test(N,allEqFormulas,(=),dprove,Prover, Culprit,Unexpected).
  


gold_classical_test(N,Silver,Culprit,Unexpected):-
  gold_test(N,allClassFormulas,(=),tautology,Silver, Culprit,Unexpected).
 
gold_full_test(N,Culprit,Unexpected):-
  gold_full_test(N,faprove,Culprit,Unexpected).
  

gold_full_test(N,Prover,Culprit,Unexpected):-
 gold_test(N,allFullFormulas,(=),dprove,Prover, Culprit,Unexpected).
 
 
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
  
:-dynamic(fof/3).


load_probs(Prover):-load_probs(compound,Prover).

load_probs(Filter,Prover):-
  max_time(M),ppp(problems_time_out_in_secs=M),nl,
  atom_codes('.',[Dot]),
  directory_files(probs,Dirs),
  findall(F,
    (
      member(Dir0,Dirs),atom_codes(Dir0,[D|_]),D=\=Dot,
      atom_concat('probs/',Dir0,Dir),
      directory_files(Dir,Fs),
      member(F0,Fs),
      atom_codes(F0,[C|_]),C=\=Dot,
      atomic_list_concat([Dir,'/',F0],F)
    ),
    Fs0
  ),
  sort(Fs0,Fs),length(Fs,Len), 
  new_ctr(Refuted),new_ctr(Wrong),new_ctr(TOut),new_ctr(Err),
  new_ctr(Skip),
  do((    
    member(InF,Fs),   
    atom_codes(InF,[C|_]),C=\=Dot,
    
    is_theorem(InF,Theo),
    load_prob(InF,GVs),
    (call(Filter,GVs)->call_prover(Prover,GVs,Res);Res=not_apply),
    ( member(Res,[true,false])->
       ( Res==Theo->ppp(InF=ok(res=Res)),(Res=false->ctr_inc(Refuted);true)
       ; ctr_inc(Wrong),ppp(InF=wrong(got=Res,should_be=Theo))
       )
    ; Res=timeout(_)->ctr_inc(TOut),ppp(InF=is(Res)+should_be(Theo))
    ; Res=not_apply->ctr_inc(Skip)
    ; ctr_inc(Err),ppp(InF=is(Res)+should_be(Theo))
    )
  )),
  ctr_get(Refuted,RK),
  ctr_get(TOut,TK),
  ctr_get(Wrong,WK),
  ctr_get(Err,EK),
  ctr_get(Skip,SK),
  Right is Len-SK-TK-WK-EK,
  Proven is Right-RK,
  Tried is Right + TK,
  ppp([
    prover=Prover,total=Len,
    skipped=SK,tried=Tried:[right=Right:[proven=Proven,refuted=RK],wrong=WK,
    timed_out(secs,M)=TK,error=EK]
  ]),
  statistics.
  
load_prob(InF,(G:-Vs)):-
   file2db(InF),
   findall(A,
     (prob:fof(_,Axiom,A),Axiom\==conjecture),
   Vs),
   prob:fof(_,conjecture,G0),
   ( G0=($true)->G=(a->a)
   ; G0=($false)->G=false
   ; G=G0
   ).

call_prover(Prover,(G:-Vs),R):-   
   max_time(MaxTime),
   unexpand(Vs,G,FullG),
   (
     timed_call(MaxTime,call(Prover,FullG),Exc) ->
     (number(Exc) -> R=true ; R=Exc)
   ; R=false
   ).
   
file2terms(F,Ts,[]):-
  read_file_to_terms(F,Ts,[]).

f2c:-is_theorem('probs/SYN391+1.pl',X),ppp(X).

is_theorem(F,true):-
  atom_codes('% Status (intuit.) : Theorem',True),
  file2comment(F,Cs),
  append(True,_,Cs),
  !.
is_theorem(_,false).  


file2comment(F,Cs):-
  atom_codes('%',[Perc]),
  file2lines(F,Ls),
  member(Cs,Ls),
  Cs=[Perc|_].
  

% reads in a file as a list of lines
file2lines(F,Ls):-
  open(F,read,S),
  get_lines(S,Ls),
  close(S).

% line to list reader helper
get_lines(S,[]):-at_end_of_stream(S),!.
get_lines(S,[Codes|Ls]):-
  read_line_to_codes(S,Codes),
  get_lines(S,Ls).
    
file2db(F):-Db=prob,
  Db:retractall(fof(_,_,_)),
  file2terms(F,Ts,_),
  do(( member(T,Ts),
    ( T=':-'(Cmd)->call(Db:Cmd)
    ; assertz(Db:T)
    )
  )).

% adaptors for othe operator sets



%map_operator(f,R)-->{!,R=false}.

map_operators(A,B):-map_operator([
  (&)-(,),
  (v)-(;),
  (<->)-(<=>),
   (->)-(=>)
],A,B).

map_operator(_,A,R):-atomic(A),!,R=A.
map_operator(Ps,A,B):-
 A=..[F|Xs],
 member(F-G,Ps),
 !,
 maplist(map_operator(Ps),Xs,Ys),
 B=..[G|Ys].
map_operator(Ps,A,B):-
 A=..[F|Xs],
 maplist(map_operator(Ps),Xs,Ys),
 B=..[F|Ys]. 
 
 