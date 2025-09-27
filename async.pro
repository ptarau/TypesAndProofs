async_init :-
   engine_create(_,dispatch_loop(Xs-Xs),_E,[alias(event_loop)]).

%% adds engine to the queue
enroll_engine(Alias):-
    ask_loop(add(Alias),_).

ask_loop(Op,Result) :-
  E=event_loop,
  engine_post(E, Op),
  engine_next(E,Result).
    
%% runs engine oerations until engine is stopped of fails
dispatch_loop(State) :-
    write(state=State),nl,
    engine_fetch(Op),
    write(op=Op),nl,
    dispatch_op(Op,R,State,NewState),
    write(new_state=NewState),nl,
    engine_yield(R),
    dispatch_loop(NewState).
 

%% dispatch operations on the engine queue
dispatch_op(add(X),added,Xs-[X|Ys], Xs-Ys).
dispatch_op(pop(X),X,[X|Xs]-Ys, Xs-Ys).
dispatch_op(list,done,Xs-Ys, Xs-Ys):-
  ( nonvar_member(X,Xs),
    write(X), nl,
    fail
  ;
    true
  ).
dispatch_op(stop, stopped, Xs-Ys, Xs-Ys):-fail.   

%% members befor a var ending the dif list
nonvar_member(H,HYs):-nonvar(HYs),[H|_]=HYs.
nonvar_member(H,[_|Ys]):-nonvar(Ys),nonvar_member(H,Ys).

% created an engine for the predicate and adds it to the queue
async(Pred):-
   functor(Pred,F,N),
   arg(N,Pred,X),
   atomic_list_concat([F,'_',N],Alias),
   not(is_engine(Alias)),
   engine_create(X,Pred,_E,[alias(Alias)]),
   enroll_engine(Alias).

%% moves one step forward in Pred - solution or yield
%% assumes Pred is scheduled and ready
await(Pred):-
   functor(Pred,F,N),
   arg(N,Pred,X),
   atomic_list_concat([F,'_',N],Alias),
   is_engine(Alias),
   engine_next(Alias, X).


foo(1).
foo(2).
foo(3).

bar(3).
bar(4).

go:-
    async_init,
    async(foo(_)),
    async(bar(_)),
    is_engine(event_loop),
    engine_post(event_loop, list,R),
    write(R), nl.
   
 ok:-
   S1=Xs-Xs,
   dispatch_op(add(boo),_,S1,S2),
   dispatch_op(add(bee),_,S2,S3),
   dispatch_op(pop(X),R,S3,S4),
   write(S3),nl,
   write(R),nl,
   write(S4),nl.
     
      
bug:-
  async_init,
  enroll_engine(foo_1),
  enroll_engine(bar_1).

  