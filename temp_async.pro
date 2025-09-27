/*
% UNFINISHED - see also  asyncio.to_thread in  Python

% source syntax
:- async p/1.

p(a).
p(b).
p(c).

q(b).
q(c).
q(d).

r(X):-await X:p(X), q(X).
*/

% E is interclausal, or if that's not available, a gensym
% or a nb global variable (but then it needs to be explictely gc-ed)

p(X):-new_engine(X,p__(X),&E_p_1).

p__(a).
p__(b).
p__(c).

q(b).
q(c).
q(d).

r(X):- await X:p_(X,&E_p_1),q(X).

await X:G :- linda_in(T),G.

% the need for a queue based evant loop vs. unification driven Linda or PubSub