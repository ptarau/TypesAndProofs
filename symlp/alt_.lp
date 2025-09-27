

% more informative

nausea(X) ; alergic_reaction(X) :- harmful(X).

bad(X) :- nausea(X).
bad(X) :- alergic_reaction(X).

-bad(benadryl).
-bad(tylenol).

take(X) :- good(X), -harmful(X).

good(X) :- -bad(X).


% ?- good(prednisone) should fail
