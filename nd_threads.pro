% thread pool mechanism for execution of 
% _nondeterministically_  generated nondeterministic goals
% a form of staged programming: a generator creates goals
% it usable to run things in constant space, nor recursion involved

test_nondet_first:-
  Gen=(
     between(1,100,I),
     Gs=(between(1,100,K),I is 100-K,I mod 10=:=0)
  ),
  Exec=Gs,   
  Sol=I,
  %Gen,(Exec),
  nondet_first(Sol,Exec,Gen), 
  ppp(sol=Sol),
  fail
; ppp(done).  
  
 
nondet_first(Sol,Exec,ExecGen):-
	thread_count(ThreadCnt),
	nondet_first_with(ThreadCnt,Sol,Exec,ExecGen).

nondet_first_with(ThreadCnt,Sol,Exec,ExecGen):- 
	%WorkSize is ThreadCnt*10,WorkOpt=[max_size(WorkSize)],
	%SolsOpt=[max_size(1)],
	WorkOpt=[],
	SolsOpt=[],
  message_queue_create(Master,WorkOpt),
  message_queue_create(Sols,SolsOpt),
  findall(Id,
    (
      between(1,ThreadCnt,_),
      thread_create(nondet_worker_with(Sols,Master),Id,[])
    ),
  Ids),
  forall(
    ExecGen,             % send as much work as generated
    thread_send_message(Master,Sol-Exec)
  ),
  thread_get_message(Sols,Sol),
  forall(
    member(Id,Ids),
    catch(thread_signal(Id, abort), _, true)
  ),
  message_queue_destroy(Master),
  message_queue_destroy(Sols). 
 
nondet_worker_with(Sols,Master):-
  repeat,
    thread_get_message(Master,Goal),
    Goal=X-G,
    G,
    !,
    thread_send_message(Sols,X),
  fail.
 
  
thread_stop:-abort.
   
send_or_switch_to_seq(ThreadCnt,Queue,Sols,Sol-Exec):-
	 message_queue_property(Queue,size(Size)),Size>ThreadCnt*ThreadCnt,
	 !,
	 once(Exec),thread_send_message(Sols,Sol).
send_or_switch_to_seq(_ThreadCnt,Queue,_Sols,Sol-Exec):-
	 thread_send_message(Queue,Sol-Exec). 
%%%%% 
  
 
nondet_run(Exec,ExecGen):-thread_count(ThreadCnt),nondet_run_with(ThreadCnt,Exec,ExecGen).

nondet_run_with(ThreadCnt,Exec,ExecGen):-  
  message_queue_create(Master,[]),
  findall(Id,
    (
      between(1,ThreadCnt,_),
      thread_create(nondet_worker(Master),Id,[])
    ),
  Ids),
  ( ExecGen,
    % send as much work as generated
    thread_send_message(Master,Exec),%ppp(sent=Exec),
    fail
  ; % send as many stops as threads, but AFTER the work is done
    forall(member(_,Ids),thread_send_message(Master,'$stop'))
  ),
  maplist(thread_join,Ids,_),
  message_queue_destroy(Master).

thread_count(ThreadCnt):-
  prolog_flag(cpu_count,MaxThreads), 
  ThreadCnt is max(2,ceiling((2/3)*MaxThreads)).
  
nondet_worker(Master):-
  repeat,
    thread_get_message(Master,Goal),
    ( Goal='$stop',!
    ; Goal,
      fail
    ).
 
    
  
nondet_run(Exec,ExecGen,Action):-nondet_run((Exec,Action),ExecGen).
% example of use and tests
  
% runs in parallel counting solutions
% uses an ExecGenerator for which runs each
% generated Goal in thread pool
       
			 
nondet_count(Exec,ExecGen,SolCount):-
	thread_count(Threads),
	nondet_count_with(Threads,Exec,ExecGen,SolCount).
				 
nondet_count_with(Threads,Exec,ExecGen,SolCount):-
  count_init,
  nondet_run_with(Threads,exec_sol_count(Exec),ExecGen),  
  count_get(SolCount).

exec_sol_count(G):-sols(G,K),count_inc(K).

count_init:-flag(sol_count,_,0).
count_inc:-flag(sol_count,K,K+1).
count_inc(C):-flag(sol_count,K,K+C).
count_get(K):-flag(sol_count,K,K).

det_count(Exec,ExecGen,SolCount):-
  count_init,
  (ExecGen,exec_sol_count(Exec),fail;true),  
  count_get(SolCount).


nondet_run1(Exec,ExecGen):-
  thread_count(ThreadCnt),Max is ThreadCnt*ThreadCnt,
  findnsols(
    Max,
    Exec,
    call(ExecGen),
    Execs
  ),
  nondet_run(Exec,member(Exec,Execs)),
  fail
; true.

nondet_run1(Exec,ExecGen,Action):-nondet_run1((Exec,Action),ExecGen).
       
nondet_count1(Exec,ExecGen,SolCount):-
  count_init,
  nondet_run1(Exec,ExecGen,count_inc),  
  count_get(SolCount).
     

nondet_test:-time(nondet_test(1000)).
  
nondet_test(N):-nondet_count(between(1,J,_),between(1,N,J),Res),writeln(Res).

det_test:-time(det_test(1000)).

det_test(N):-det_count(between(1,N,J),between(1,N,J),Res),writeln(Res).
  

   