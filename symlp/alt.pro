:-include('compile_clauses.pro').

% negative

a => b ; c.

b => f.
c => f.

-f.

% positive

p <= q, false:a.

q <= false:b.

% more informative example

% falsifiable negative part

harmful(X) => nausea(X) ; alergic_reaction(X).

nausea(X) => bad(X).

alergic_reaction(X) => bad(X).

-bad(benadryl).
-bad(tylenol).

% positive calling out for bindings in module false

take(X) <= good(X), false:harmful(X).

good(X) <= false:bad(X).

% ?- true:good(X).
%   X = benadryl ;
%   X = tylenol.

% ?- good(prednisone) should fail
