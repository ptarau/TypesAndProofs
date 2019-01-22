p1:- do((s_(T),varvars(T,TT),(qqq(TT);ppt(TT)))).

pph(Horns):-is_list(Horns),!,
  do((
    member(H,Horns),
    pph(H),nl
  )),
  nl.
  
pph(Horn):-horn2term(Horn,Term),ppt(Term).

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
    
showFlatHorn:-
 tell('docs/horn3big.txt'),
 showFlatHorn(9),
 told.

showFlatHorn(N):-
  do((
    allSortedHorn(N,T0),  
    ppp(before=T0),nl,
    pph(T0),nl,
    (ljh(T0)->R0=true;R0=false),
    flattenHorn(T0,T),
    ppp(after_=T),nl,
    pph(T),nl,
    hdepth(T,D),
    (ljh(T)->R=true;R=false),
    ppp([taut=R0+R,depth=D]),
    ppp('--------------------------'),
    nl,
    assertion(R0=R),
    assertion(D=<3)
    )).  
  
 showFlatImp(N):-
  do((
    allSortedHorn(N,T0),
    toHorn(A0,T0),
    ppp(A0),
    %ppt(A0),
    nl,
    flattenHorn(T0,T),
    toHorn(A,T),
    ppp(A),
    ppt(A),
    left_depth(A,D),assertion(D=<3),
    nl,
    (D>=3->ppp(D);true),
    ppp('------------')
    )).  
    
left_depth(A,R):-(atomic(A);var(A)),!,R=0.
left_depth(A->_,R):-left_depth(A,D),R is D+1.

showSortedHorns(N):-
  do((
     
     allSortedHorn(N,T),
     ppp(T),
     horn2term(T,Pro),
     pph(T),    
     ppp(Pro),
     nl
    )).
