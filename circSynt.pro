% synthesizes smallest implicational formula matching a specification
% given as a full IPL formula


atomic_vars_of(T,Vs,Us):-vars_of(T,Vs,[]),sort(Vs,Us).

vars_of(A)-->{atomic(A)},!,[A].
vars_of(T)-->{T=..[_|Xs]},map_vars_of(Xs).

map_vars_of([])-->[].
map_vars_of([X|Xs])-->vars_of(X),map_vars_of(Xs).

syn(Prover,Ops,Spec, Formula):-
  atomic_vars_of(Spec,Vs,Us),
  length(Vs,Max0),length(Us,Min0),
  Min  is Min0-1,
  Max is max(8,2*Max0),
  between(Min,Max,N),
  %genTree(N,Formula,Ws),
  genOpTree(N,Ops,Formula,Ws),
  mpart_of(Ws,Us),
  %ppp(Formula),
  call(Prover,Spec<->Formula).

% synthesizes an classical implicational formula equivalet to Spec  
tsyn(Ops,Spec,Formula):-syn(tautology, Ops, Spec, Formula),!.

tsyn(Spec,Formula):-tsyn([(<->),(v)],Spec,Formula).

% same for an intuitionistc one, when possible
% often this will fail, as intuitionistic operators
% cannot always be expressed in terms of others
isyn(Ops,Spec,Formula):-syn(dprove, Ops, Spec, Formula),!.



isyn(Spec,Formula):-isyn([(<->),(v)],Spec,Formula).


jsyn(Ops,Spec,Formula):-syn(faprove, Ops, Spec, Formula),!.

jsyn(Spec,Formula):-jsyn([(<->),(v)],Spec,Formula).

csyn(Spec,Formula):-jsyn([(<->),(->)],Spec,Formula).


true_fact:-faprove((a&b <-> ((a->b)<->a))).

no_synbug:-isyn(a & b, R),ppp(R).

syn_test:-T=(a -> b&c),csyn(a -> b&c, R),ppp(T=R).