:-dynamic(mustStop/0).

%fpprove(T0):-fpprove(T0,R),(R=timeout(_)->throw(R);R).

fpprove(T0,R):-
  expand_full_neg(T0,T),
  ljfp__(T,R).


ljfp__(T,R):-
   max_time(Secs),
   retractall(logged(_)),
   retractall(mustStop),
   retractall(stoppedTimer),
  ((
    %alarm(Secs,bg(assertz(mustStop)),_Id,[remove(true)]),
    timer(Secs,stoppedTimer,assertz(mustStop),Id)
  )),
  ((
    ljfp_(Secs,T,R),
    assertz(stoppedTimer)
  )),
  fg(Id,_).
 
ljfp_(Secs,T,R):-
 (
   catch(ljfp(T),Exc,true)->
     ( var(Exc)->R0=true
     ; R0=Exc
       ,log(Exc)
     )
 ; R0=false
 ),
 ( mustStop->R=timeout(Secs)
 ; R=R0
 ).

:-dynamic stoppedTimer/0.

timer(Secs,Stopper,Goal,Id):-
  bg(run_timer(Secs,Stopper,Goal),Id).

run_timer(Secs,Stopper,Goal):-N=20,
  Secs0 is Secs/N,
  do((
    between(1,N,_),
    \+ Stopper,
    sleep(Secs0)
  )),
  do((
    \+ Stopper,
    once(Goal)
  )).




bg(G):-thread_create(G,_Id,[detached(true)]).
 
bg(G,Id):-
  thread_create(G,Id,[
  % stack_limit(Lim)
  ]).
 
fg(Id,R):-thread_join(Id,R).


:-dynamic(logged/1).
log(Mes):-assertz(logged(Mes)).
lg:-do logged(X),ppp(X) .

 
%andPar(A,B):-A,B,!.
andPar(A,B):-bg(A,I),bg(B,J),fg(I,RA),fg(J,RB),RA,RB.

exception(E):-throw(E).

%andPar(A,B):-A,B->ppp(true(A+B));ppp(false(A+B)),fail.
%andPar(A,B):-bg(A,I),bg(B,J),fg(I,RA),fg(J,RB),RA,RB->ppp(true(A+B));ppp(false(A+B)),fail.

%andPar(A,B):-concurrent(2,[A,B],[]).

orPar(A,B):-!,(A;B),!.
orPar(A,B):-
  %ppp((A ; B)),
  first_solution(S,[(A,S=a),(B,S=B)],[on_failure(continue)])->true
  %memberchk(S,[a,b]),
  %ppp(res=S)
  ; 
  %ppp(failed((A;B))),
  fail.

/*
orPar(A,B):-
   message_queue_create(Q,[max_size(2)]),
   bg((once(A),thread_send_message(Q,a)),IdA),
   bg((once(B),thread_send_message(Q,b)),IdB),
   max_time(T),
   ( thread_get_message(Q,Mes,[timeout(T)])->
       ( Mes=a->fg(IdA,R),thread_stop(IdB),fg(IdB,_)
       ; Mes=b->fg(IdB,R),thread_stop(IdA),fg(IdA,_)
       ; R=throw(unexpected(Mes))
       )
   ; thread_stop(IdA),fg(IdA,_),
     thread_stop(IdB),fg(IdB,_),
     fail
   ),
   message_queue_destroy(Q),
   R.

:-dynamic message_hook/3.
:-multifile message_hook/3.
message_hook(_,_,_).
*/

thread_stop(Id):-catch(
   thread_signal(Id, throw(timeout(66))),
   _Exc,
   true).
 
ljfp(T):-ljfp(T,[]),!.
 
%jfp(A,Vs):-ppp(ljfp:(Vs-->A)),fail. % for tracing only

ljfp(_A,_Vs):-mustStop,!,timeout.

ljfp(A,Vs):-memberchk(A,Vs),!.
ljfp(_,Vs):-memberchk(false,Vs),!.
ljfp(A <-> B,Vs):-!,andPar(ljfp((A->B),Vs),ljfp((B->A),Vs)).
ljfp((A->B),Vs):-!,ljfp(B,[A|Vs]). 
ljfp(A & B,Vs):-!,andPar(ljfp(A,Vs),ljfp(B,Vs)).
ljfp(G,Vs1):-
  select(Red,Vs1,Vs2),
  ljfp_reduce(Red,G,Vs2,Vs3),
  !,
  ljfp(G,Vs3).
ljfp(A v B, Vs):-orPar(ljfp(A,Vs),ljfp(B,Vs)).

%ljfp_reduce(AB,B,Vs,Vs):-ppp(ljfp_reduce:(vs:Vs-->ab:AB+b:B)),fail. 


%ljfp_reduce(_,_,_,_):-mustStop,!,timeout.

ljfp_reduce((A->B),_,Vs1,Vs2):-!,ljfp_imp(A,B,Vs1,Vs2).
ljfp_reduce((A & B),_,Vs,[A,B|Vs]):-!.
ljfp_reduce((A<->B),_,Vs,[(A->B),(B->A)|Vs]):-!.
ljfp_reduce((A v B),G,Vs,[B|Vs]):-ljfp(G,[A|Vs]).
  

%ljfp_imp(_,_,_,_):-mustStop,!,timeout.

ljfp_imp((C-> D),B,Vs,[B|Vs]):-!,ljfp((C->D),[(D->B)|Vs]).
ljfp_imp((C & D),B,Vs,[(C->(D->B))|Vs]):-!.
ljfp_imp((C v D),B,Vs,[(C->B),(D->B)|Vs]):-!.
ljfp_imp((C <-> D),B,Vs,[((C->D)->((D->C)->B))|Vs]):-!.
ljfp_imp(A,B,Vs,[B|Vs]):-memberchk(A,Vs).  

timeout:-fail.






%timeout:-fail.

/*

%bgfg(G):-fg(G,Id),bg(Id,R),R.

join_my_threads(Secs):- 
  do
    thread_property(Id,_),
    \+ member(Id,[main,gc]),
    thread_signal(Id,thread_exit(timeout(Secs))),
    thread_join(Id,_).
    
    
stop_all_threads:-
  do((
    thread_pool_property(parPool,running(Ids)),
    member(Id,Ids),
    thread_stop(Id)
  )). 

force_fail:-
  thread_self(Id),
  \+member(Id,[main,gc]),
  thread_exit(fail),
  fail.

thread_stop(main):-!.
thread_stop(gc):-!.
thread_stop(Id):-thread_signal(Id, thread_exit(fail)).


initPool:-
  thread_count(K),
  thread_pool_create(parPool,K,[]).
  
endPool:-catch(endPool0,_,true).

endPool0:-stop_all_threads,thread_pool_destroy(parPool).

bg(G,Id):-
  thread_pool_property(parPool,free(I)),I>0,!,
  % Lim is 2^29,
  thread_create_in_pool(parPool,once(G),Id,[
  % stack_limit(Lim)
  ]).
bg(G,seq):-once(G).
 
fg(seq):-!.
fg(Id):-thread_join(Id,[]).

ljfp(T):-
  S=state(true),
  (ljfp_(T,[],S)->Ok=true;Ok=fail),
  arg(1,S,Res),
  ( Res=true->Ok
  ; throw(Res)
  ).
  
%ljfp(A,Vs,S):-ppp(ljfp:(Vs:-A)=S),fail. % fo traing only

  
ljfp(A,Vs,_):-memberchk(A,Vs),!.
ljfp(_,Vs,_):-memberchk(false,Vs),!.
ljfp((A->B),Vs,S):-!,ljfp_(B,[A|Vs],S). 
ljfp(G,Vs1,S):-
  select(Red,Vs1,Vs2),
  ljfp_reduce(Red,G,Vs2,Vs3,S),
  !,
  ljfp_(G,Vs3,S).
ljfp(A <-> B,Vs,S):-!,andPar1((A->B),(B->A),Vs,S).
ljfp(A & B,Vs,S):-!,andPar(A,B,Vs,S).
ljfp(A v B, Vs,S):-orPar(A,B,Vs,S).


%ljfp_reduce(AB,B,Vs,Vs,S):-ppp(ljfp_reduce(AB,B,Vs,Vs,S)),fail. 

ljfp_reduce((A->B),_,Vs1,Vs2,S):-!,ljfp_imp(A,B,Vs1,Vs2,S).
ljfp_reduce((A & B),_,Vs,[A,B|Vs],_):-!.
ljfp_reduce((A<->B),_,Vs,[(A->B),(B->A)|Vs],_):-!.
ljfp_reduce((A v B),G,Vs,[B|Vs],S):-ljfp_(G,[A|Vs],S).

ljfp_imp((C-> D),B,Vs,[B|Vs],S):-!,ljfp_((C->D),[(D->B)|Vs],S).
ljfp_imp((C & D),B,Vs,[(C->(D->B))|Vs],_):-!.
ljfp_imp((C v D),B,Vs,[(C->B),(D->B)|Vs],_):-!.
ljfp_imp((C <-> D),B,Vs,[((C->D)->((D->C)->B))|Vs],_):-!.
ljfp_imp(A,B,Vs,[B|Vs],_):-memberchk(A,Vs).  

ljfp_(A,Vs,S):-
  arg(1,S,true),
  max_time(Secs),
  Goal=ljfp(A,Vs,S),
  catch(call_with_time_limit(Secs, Goal), Exc, true),
  ( var(Exc)->true
  ; nb_setarg(1,S,Exc)
  ).
  
andPar(A,B,Vs,S):-ljfp_(A,Vs,S),ljfp_(B,Vs,S).

andPar2(A,B,Vs,S):-
  S0=S,S1=S,
  concurrent(2,
     [ljfp_(A,Vs,S0),ljfp_(B,Vs,S1)],
     []
  ),
  arg(1,S0,R0),arg(1,S1,R1),
  ( R0=true,R1=true->true
  ; R0=true->nb_setarg(1,S,R1)
  ; nb_setarg(1,S,R0)
  ).
  

orPar(A,B,Vs,S):-ljfp_(A,Vs,S);ljfp_(B,Vs,S).

orPar(A,B,Vs,S):-
   S0=state(true),
   first_solution(S0,[ljfp_(A,Vs,S0),ljfp_(B,Vs,S0)],
    [
     % on_failure(continue)
    ]
   ),
   arg(1,S0,Res),
   nb_setarg(1,S,Res).
   
andPar(A,B,Vs,N0,N4):-succ(N0,N1),
  concurrent(2,
     [timed_ljfp(A,Vs,N1,N2),timed_ljfp(B,Vs,N1,N3)],
     []
  ),
  N4 is N1+(N2-N1)+(N3-N1).


orPar(A,B,Vs,N0,N2):-succ(N0,N1),
  first_solution(_,
     [timed_ljfp(A,Vs,N1,N2),timed_ljfp(B,Vs,N1,N2)],
     [on_failure(continue)]
  ).


andPar(A,B,Vs,N,N):-
    timed_ljfp(A,Vs,N,N),
    timed_ljfp(B,Vs,N,N).

andPar1(A,B,Vs,N,N):-
  bg(timed_ljfp(B,Vs,N,N),Id),
  timed_ljfp(A,Vs,N,N),
  fg(Id).
  
orPar1(A,B,Vs,N,N) :-
   timed_ljfp(A,Vs,N,N)
;  timed_ljfp(B,Vs,N,N).
  
initPool:-
  thread_count(K),
  thread_pool_create(parPool,K,[]).
  
endPool:-thread_pool_destroy(parPool).

bg(G,Id):-
  Lim is 2^29,
  thread_create_in_pool(parPool,G,Id,[
   stack_limit(Lim)
  ]).

fg(Id):-thread_join(Id,R),
  ( R=true->true
  ; R=false->fail
  ; R=exception(T),throw(T)
  ).

  
  

timed_ljfp(G,Vs,N1,N2):-
   max_time(Secs),
   call_with_time_limit(Secs, ljfp(G,Vs,N1,N2)).

*/


scrambleFormula(A&B,X&Y):-!,ranFlip(A,B,X,Y).
scrambleFormula(A v B,X v Y):-!,ranFlip(A,B,X,Y).
scrambleFormula(A<->B,X<->Y):-!,ranFlip(A,B,X,Y).
scrambleFormula(A->B->C,X->Y->C):-!,ranFlip(A,B,X,Y).
scrambleFormula(~ A , ~ X):-!,scrambleFormula(A,X).
scrambleFormula(A->B,X->Y):-!,
  scrambleFormula(A,X),
  scrambleFormula(B,Y).
scrambleFormula(A,A).  
  
ranFlip(A,B,X,Y):-
  scrambleFormula(A,SA),
  scrambleFormula(B,SB),
  ranFlip1(SA,SB,X,Y).
  
ranFlip1(A,B,X,Y):-0=:=random(2),!,X=A,Y=B.
ranFlip1(A,B,B,A).


callOrFail(Time,P,F,R):-
  timed_call(Time,call(P,F),Answer)->
  (atomic(Answer)->R=true;R=Answer)
  ; R=false.
  
  
  
   
%callOrFail(_P,_F,false).

parFullProver(Time,Seed,Prover,Formula):-
  set_random(seed(Seed)),
  thread_count(ThreadCnt),
  ExecGen=(
    between(1,ThreadCnt,_),
    scrambleFormula(Formula,Scrambled)
    %ppp(Scrambled)
  ),
  Exec=callOrFail(Time,Prover,Scrambled,Sol),
  %ppp(starting),
  nondet_first_with(ThreadCnt,Sol,Exec,ExecGen),
  %ppp(sol=Sol),
  Sol\=false.
   
par_faprove(Formula):-
  Seed=42,
  max_time(Time),
  Prover=faprove,
  parFullProver(Time,Seed,Prover,Formula). 
  
par_dprove(Formula):-
  Seed=42,
  max_time(Time),
  Prover=dprove,
  parFullProver(Time,Seed,Prover,Formula). 
