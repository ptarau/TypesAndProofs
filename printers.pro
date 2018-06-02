p1:- do((s_(T),varvars(T,TT),(qqq(TT);ppt(TT)))).

 
showImpForms(N):-
  do((
    genTree(N,T,Vs),
    vpartitions(Vs),
    ppp(T)
    )).
    
showSortedHorn(N):-
  do((
    allSortedHorn(N,T),
    ppt(T),
    hdepth(T,D),
    ppp(D),
    nl
  )).
    

showFlatHorn(N):-
  do((
    allSortedHorn(N,T0),  
    ppp(before=T0),nl,
    ppt(T0),nl,
    (ljh(T0)->R0=true;R0=false),
    flattenHorn(T0,T),
    ppp(after_=T),nl,
    ppt(T),nl,
    hdepth(T,D),
    (ljh(T)->R=true;R=false),
    ppp([taut=R0+R,depth=D]),
    ppp('--------------------------'),
    nl,
    assertion(R0=R),
    assertion(D=<3)
    )).  
  