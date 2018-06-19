% synthesizes smallest implicational formula matching a specification
% given as a full IPL formula


atomic_vars_of(T,Vs,Us):-vars_of(T,Vs,[]),sort(Vs,Us).

vars_of(A)-->{atomic(A)},!,[A].
vars_of(T)-->{T=..[_|Xs]},map_vars_of(Xs).

map_vars_of([])-->[].
map_vars_of([X|Xs])-->vars_of(X),map_vars_of(Xs).

syn(Prover, Spec, Formula):-
  atomic_vars_of(Spec,Vs,Us),
  length(Vs,Max0),length(Us,Min0),
  Min  is Min0-1,
  Max is 2*Max0,
  between(Min,Max,N),
  genTree(N,Formula,Ws),
  mpart_of(Ws,Us),
  call(Prover,Spec<->Formula).

% synthesizes an classical implicational formula equivalet to Spec  
tsyn(Spec,Formula):-syn(tautology, Spec, Formula),!.

% same for an intuitionistc one, when possible
% often this will fail, as intuitionistic operators
% cannot always be expressed in terms of others
isyn(Spec,Formula):-syn(faprove, Spec, Formula),!.
