:-include('tp.pro').

prop1a:-
   T1=(p0 v ~p1 v ~pn),
   T2=(p0 <- p1 & pn),
   iprover(T1->T2),
   cprover(T1<->T2).
   
prop1b:-
   T1=(~p0 v p1 v pn),
   T2=(p0 -> p1 v pn),
   iprover(T1->T2),
   cprover(T1<->T2).  
   
prop2a:-
   T1=(~p1 v ~pn),
   T2=(false <- p1 & pn),
   iprover(T1->T2),
   cprover(T1<->T2).
   
prop2b:-
   T1=(p1 v pn),
   T2=(true -> p1 v pn),
   iprover(T1<->T2),
   cprover(T1<->T2).  

contrapos:-
   T1=( a -> b) ,
   T2=(~b -> ~a),
   iprover(T1->T2),
   cprover(T1<->T2).
   
props:-
  prop1a,
  prop1b,
  prop2a,
  prop2b,
  contrapos.

  