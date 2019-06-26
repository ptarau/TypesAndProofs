:-include('../SOFTWARE/PROVERS/fCube-4.1/fCube/fCube').

fcube(A):-toPrefix(A,X),fc(X).

fc(X):-intDecide0(X,_).

intDecide0(X,COUNTERMODEL):-	
  permanenzaSegno([swff(f,X)],StartingSet),
  orderEquivSet(StartingSet, OrderedStartingSet),
  reapply(OrderedStartingSet, COUNTERMODEL, 1, 1),!,
  fail.
intDecide0(_,[valida]).

notfc:-T0=(p <-> ~q)<-> (~q <-> p),ppp(T),
  mints(T0,T),ppp(T),faprove(T),dprove(T).

fcbug:-T0=(p <-> ~q)<-> (~q <-> p),fsize(T0,S0),
   fcube(T0),ppp(should_be_true(S0,T0)),nl,
   mints(T0,T),fsize(T,S),
   ppp(trying_equivalent(S)=T),
   fcube(T).
   
fcbug1:-
  T0=((p <-> ~q)-> (~q -> p)),
   fcube(T0),ppp(should_be_true(T0)),
   mints(T0,T),
   ppp(trying_equivalent=T),
   fcube(T).
  
fcbug2:-
  T0=((~p <-> q)-> (q -> ~p)),
  mints(T0,T),
  fcube(T0),ppp(should_be_true(T0)),
  ppp(trying_equivalent=T),
  fcube(T).


 
fcbugs:-fcbugs(5).

mints_fcube(A):-mints(A,MA),fcube(MA).

fcbugs(N):-
 do((
   gold_eq_neg_test(N,mints_fcube,Culprit,Unexpected),ppp([Unexpected,Culprit])
 )).


 
mints_dprove(A):-mints(A,MA),dprove(MA).

mints_faprove(A):-mints(A,MA),faprove(MA).

mints_coprove(A):-mints(A,MA),coprove(MA).

mints_eprove(A):-mints(A,MA),eprove(MA).

mints_impl_taut(N,T,MT):-implTaut(N,T),mints(T,MT).

alt_impl_taut(N,T,AT):-implTaut(N,T),toDisjBiCond(T,AT).


small_taut_bug(M,Prover):-
 do((
   between(0,M,N),
   mints_impl_taut(N,T,MT),
   \+ call(Prover,MT),
   ppp(unexpected_failure_on),
   ppp(T),
   ppp((<=>)),
   ppp(MT),nl
 )).
  

 
alt_small_taut_bug(M,Prover):-
 do((
   between(0,M,N),
   alt_impl_taut(N,T,MT),
   ppp(trying=T),
    ppp(as=MT),
   \+ call(Prover,MT),
   ppp(unexpected_failure_on),
   ppp(T),
   ppp((<=>)),
   ppp(MT),nl
   )).
   

dnobugs(N):-
 do((
   gold_eq_neg_test(N,mints_dprove,Culprit,Unexpected),ppp([Unexpected,Culprit])
   )).



smallfcbugs:-ppp(fcube_at_4_bugs),
 do((
  between(0,4,N),gold_imp_test(N,mints_fcube,C,U),
  ppp([N,U,C]),
  mints(C,MC),fsize(MC,S),
  ppp(culprit_size(S)=MC),nl
 )).
 
nosmallbugs:-ppp(eprove_up_to_5_ok),
  do((
  between(0,5,N),gold_imp_test(N,mints_eprove,C,U),
  ppp([N,U,C]),
  mints(C,MC),fsize(MC,S),
  ppp(culprit_size(S)=MC),nl
  )).
  
/*
 
 ?- smallfcbugs.
fcube_at_4_bugs
[4,wrong_failure,(0->1->2->2->0)]
culprit_size(24)=((nv1->0->nv2)->(nv2->1->nv3)->(nv3->2->nv4)->(nv4->2->0)->((0->nv2)->nv1)->((1->nv3)->nv2)->((2->0)->nv4)->((2->nv4)->nv3)->nv1)

[4,wrong_failure,(0->1->2->3->0)]
culprit_size(24)=((nv1->0->nv2)->(nv2->1->nv3)->(nv3->2->nv4)->(nv4->3->0)->((0->nv2)->nv1)->((1->nv3)->nv2)->((2->nv4)->nv3)->((3->0)->nv4)->nv1)

[4,wrong_failure,(0->(1->2->0)->0)]
culprit_size(24)=((nv1->0->nv2)->(nv2->nv3->0)->(nv3->1->nv4)->(nv4->2->0)->((0->nv2)->nv1)->((1->nv4)->nv3)->((2->0)->nv4)->((nv3->0)->nv2)->nv1)

[4,wrong_failure,(0->(1->0->2)->0)]
culprit_size(24)=((nv1->0->nv2)->(nv2->nv3->0)->(nv3->1->nv4)->(nv4->0->2)->((0->2)->nv4)->((0->nv2)->nv1)->((1->nv4)->nv3)->((nv3->0)->nv2)->nv1)

[4,wrong_failure,(0->(1->2->3)->0)]
culprit_size(24)=((nv1->0->nv2)->(nv2->nv3->0)->(nv3->1->nv4)->(nv4->2->3)->((0->nv2)->nv1)->((1->nv4)->nv3)->((2->3)->nv4)->((nv3->0)->nv2)->nv1)

true.


 ?- fcbugs(5).
[wrong_failure,~ (0<->(1<-> ~ (1<->0)))]
[wrong_failure,~ (0<->(1<-> ~ (0<->1)))]
[wrong_failure,~ (0<->(1<->(0<-> ~1)))]
[wrong_failure,~ (0<->(0<->(1<-> ~1)))]
[wrong_failure,~ (0<->(1<->(~1<->0)))]
[wrong_failure,~ (0<->(0<->(~1<->1)))]
[wrong_failure,~ (0<->(~ (1<->0)<->1))]
[wrong_failure,~ (0<->(~ (0<->1)<->1))]
[wrong_failure,~ (0<->((1<-> ~1)<->0))]
[wrong_failure,~ (0<->((0<-> ~1)<->1))]
[wrong_failure,~ (0<->((~1<->1)<->0))]
[wrong_failure,~ (0<->((~1<->0)<->1))]
[wrong_failure,~0<->(1<->(1<-> ~0))]
[wrong_failure,~0<->(1<->(~0<->1))]
[wrong_failure,~0<->(~1<->(1<->0))]
[wrong_failure,~0<->(~1<->(0<->1))]
[wrong_failure,~0<->((1<->0)<-> ~1)]
[wrong_failure,~0<->((0<->1)<-> ~1)]
[wrong_failure,~0<->((1<-> ~0)<->1)]
[wrong_failure,~0<->((~0<->1)<->1)]
[wrong_failure,~ ((0<->1)<->(1<-> ~0))]
[wrong_failure,~ ((0<->1)<->(0<-> ~1))]
[wrong_failure,(0<->1)<->(0<->(0<->(1<->0)))]
[wrong_failure,(0<->1)<->(1<->(1<->(1<->0)))]
[wrong_failure,(0<->1)<->(0<->(0<->(0<->1)))]
[wrong_failure,(0<->1)<->(1<->(1<->(0<->1)))]
[wrong_failure,(0<->1)<->(0<->((1<->0)<->0))]
[wrong_failure,(0<->1)<->(0<->((0<->1)<->0))]
[wrong_failure,(0<->1)<->(1<->((1<->0)<->1))]
[wrong_failure,(0<->1)<->(1<->((0<->1)<->1))]
[wrong_failure,~ ((0<->1)<->(~1<->0))]
[wrong_failure,~ ((0<->1)<->(~0<->1))]
[wrong_failure,(0<->1)<->((0<->(1<->0))<->0)]
[wrong_failure,(0<->1)<->((0<->(0<->1))<->0)]
[wrong_failure,(0<->1)<->((1<->(1<->0))<->1)]
[wrong_failure,(0<->1)<->((1<->(0<->1))<->1)]
[wrong_failure,(0<->1)<->(((1<->0)<->0)<->0)]
[wrong_failure,(0<->1)<->(((0<->1)<->0)<->0)]
[wrong_failure,(0<->1)<->(((1<->0)<->1)<->1)]
[wrong_failure,(0<->1)<->(((0<->1)<->1)<->1)]
[wrong_failure,~ ((0<-> ~1)<->(1<->0))]
[wrong_failure,~ ((0<-> ~1)<->(0<->1))]
[wrong_failure,~ ((0<-> ~ (1<->0))<->1)]
[wrong_failure,~ ((0<-> ~ (0<->1))<->1)]
[wrong_failure,~ ((0<->(1<-> ~1))<->0)]
[wrong_failure,~ ((0<->(0<-> ~1))<->1)]
[wrong_failure,(0<->(0<->(1<->0)))<->(1<->0)]
[wrong_failure,(0<->(0<->(0<->1)))<->(1<->0)]
[wrong_failure,(0<->(0<->(1<->0)))<->(0<->1)]
[wrong_failure,(0<->(0<->(0<->1)))<->(0<->1)]
[wrong_failure,~ ((0<->(~1<->1))<->0)]
[wrong_failure,~ ((0<->(~1<->0))<->1)]
[wrong_failure,(0<->((1<->0)<->0))<->(1<->0)]
[wrong_failure,(0<->((0<->1)<->0))<->(1<->0)]
[wrong_failure,(0<->((1<->0)<->0))<->(0<->1)]
[wrong_failure,(0<->((0<->1)<->0))<->(0<->1)]
[wrong_failure,~ ((~0<->1)<->(1<->0))]
[wrong_failure,~ ((~0<->1)<->(0<->1))]
[wrong_failure,~ ((~ (0<->1)<->1)<->0)]
[wrong_failure,~ ((~ (0<->1)<->0)<->1)]
[wrong_failure,~ (((0<-> ~1)<->1)<->0)]
[wrong_failure,~ (((0<-> ~1)<->0)<->1)]
[wrong_failure,~ (((0<-> ~0)<->1)<->1)]
[wrong_failure,((0<->(1<->0))<->0)<->(1<->0)]
[wrong_failure,((0<->(0<->1))<->0)<->(1<->0)]
[wrong_failure,((0<->(1<->0))<->0)<->(0<->1)]
[wrong_failure,((0<->(0<->1))<->0)<->(0<->1)]
[wrong_failure,~ (((~0<->0)<->1)<->1)]
[wrong_failure,(((0<->1)<->0)<->0)<->(1<->0)]
[wrong_failure,(((0<->1)<->1)<->1)<->(1<->0)]
[wrong_failure,(((0<->1)<->0)<->0)<->(0<->1)]
[wrong_failure,(((0<->1)<->1)<->1)<->(0<->1)]
true.

?- 

 */