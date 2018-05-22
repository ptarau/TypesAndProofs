% benchmarks prover P on terms of size N

xbm0:-xbm(10,12).

xbm:-tell('bm.txt'),xbm(13,15),told.

xbm(From,To):-
  member(P,[lprove,bprove,sprove,pprove,hprove,xprove]),
  nl,between(From,To,N),
    bm(N,P),
  fail
; nl,between(From,To,N),P=dprove,
  timed_call(600,bm(N,P),Time),
  (number(Time)->true;ppp(P=Time)),
  fail
; member(P,[kprove,gprove,tautology]),
  nl,between(From,To,N),
    cbm(N,P),
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
