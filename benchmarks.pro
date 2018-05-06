% benchmarks prover P on terms of size N

xbm0:-xbm(10,12).

xbm:-tell('bm.txt'),xbm(13,15),told.

xbm(From,To):-
  member(P,[lprove,bprove,sprove,pprove]),
  nl,between(From,To,N),
    bm(N,P),
  fail
; member(P,[hprove,xprove]),
  nl,between(From,To,N),
   hbm(N,P),
  fail
; nl,between(From,To,N),P=dprove,
  timed_call(600,bm(N,P),Time),
  (number(Time)->true;ppp(P=Time)),
  fail
; nl,writeln(done).
  
timed_call(Secs,Goal,Time):-
  get_time(T0),
  catch(
      call_with_time_limit(Secs,Goal),
      Exc,
      true
  ),
  get_time(T1),
  (var(Exc) -> Time is T1-T0 ; Time = timeout_after(Secs)).
  
bm(N,P):-
  N1 is N//2,
  pbm(N,P,PT), % on all positive examples
  nbm(N1,P,NT), % on a blend, mostly negative examples
  T is PT+NT,
  maplist(nice_num,
     [N,PT,NT,T],
     [NN,PT1,NT1,T1]
  ),
  writeln([prog=P,size=NN,pos=PT1,neg=NT1,total=T1]).

  
nice_num(X,R):-R is (truncate(X*100))/100.

/*

?- hbm(16,hprove).
[prog=hprove,size=16,pos=88.26,neg=105.37,total=193.64]
true.

?- hbm(17,hprove).
[prog=hprove,size=17,pos=433.95,neg=109.58,total=543.54]
true.

?- hbm(18,hprove).
[prog=hprove,size=18,pos=2122.84,neg=2413.12,total=4535.96]
*/

hbm(N,P):-
  N1 is N//2,
  hpbm(N,P,PT), % on all positive examples
  hnbm(N1,P,NT), % on a blend, mostly negative examples
  T is PT+NT,
  maplist(nice_num,
     [N,PT,NT,T],
     [NN,PT1,NT1,T1]
  ),
  writeln([prog=P,size=NN,pos=PT1,neg=NT1,total=T1]). 
  
  % benchmark on type of normal forms of size N 
pbm(N,P,Time):-
  time(pbm0(N),T0),
  time(ptest(N,P),T1),
  Time is T1-T0.
  
pbm(N,P):-
  pbm(N,P,Time),
  writeln(time(pbm)=Time).
  
% just the terms, for testing, to help with
% bencharkin g exact time spent in proving
pbm0(N):-
  tnf(N,_:T),
  natvars(T),
  fail
; true.

hpbm0(N):-
  tnf(N,_:T),
  natvars(T),
  toHorn(T,_),
  fail
; true.

% same, counting Horn clause transformer time
hpbm(N,P,Time):-
  time(hpbm0(N),T0),
  time(ptest(N,P),T1),
  Time is T1-T0.  

hpbm(N,P):-
  hpbm(N,P,Time),
  writeln(time(pbm)=Time).  

% banchmark with binary 
% trees labeled in all possible ways  

hnbm(N,P,Time):-
  time(hnbm0(N),T0),
  time(hnbm1(N,P),T1),
  Time is T1-T0.

hnbm(N,P):-
  hnbm(N,P,Time),
  writeln(time(nbm)=Time).

hnbm1(N,P):-
  call(allImpFormulas,N,T0),
  toHorn(T0,T),
  call(P,T),
  fail
  ; true.
  

hnbm0(N):-
  call(allImpFormulas,N,T),toHorn(T,_),fail;true.


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




% test against Fitting's prover as gold standard
% needs replacing the reduced one with :-include(..orig...)
% ?-cntest(6,taut).
% >-cntest(6,cprove).

cntest(N,P):-
  allClassFormulas(N,T), %ppp(T),
  ( call(P,T) -> \+tautology(T),ppp(false_pos(T))
  ; dprove(T) -> tautology(T),ppp(false_neg(T))
  ), 
  fail
; true.

do(Goal):-
  Goal,
  fail
; true.

do(G1,G2):-
  G1,G2,
  fail
; true.

do(G1,G2,G3):-
  G1,G2,G3,
  fail
; true.

do(G1,G2,G3,G4):-
  G1,G2,G3,G4,
  fail
; true.

cbm(N,P):-
  time(sols(allClassFormulas(N,T),Sols),Time0),
 
  time(
    do(
      allClassFormulas(N,T),
      call(P,T)
    ),
  Time1),
  Time2 is Time1-Time0,
  nice_num(Time2,Time),
  ppp([n=N,prog=P,sols=Sols,time=Time]).

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
