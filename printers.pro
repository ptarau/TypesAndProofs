p1:- do((s_(T),varvars(T,TT),(qqq(TT);ppt(TT)))).

 
showImpForms(N):-
  do((
    genTree(N,T,Vs),
    vpartitions(Vs),
    ppp(T)
    )).
    
