% benchmarks prover P on terms of size N

bm(N,P):-
  N1 is N//2,
  pbm(N,P,PT), % on all positive examples
  nbm(N1,P,NT), % on a blend, mostly negative examples
  T is PT+NT,
  writeln(time=[p=PT,n=NT,total=T]).

hbm(N,P):-
  N1 is N//2,
  hpbm(N,P,PT), % on all positive examples
  hnbm(N1,P,NT), % on a blend, mostly negative examples
  T is PT+NT,
  writeln(time=[p=PT,n=NT,total=T]).  
  
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
