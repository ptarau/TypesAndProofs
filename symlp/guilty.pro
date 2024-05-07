:-include('compile_clauses.pro').

exonerated(X) <= suspect(X),false:guilty(X).

investigated(X) <= suspect(X),not(false:guilty(X)).

+suspect(alice).
+suspect(bob).

guilty(X) => found_of(X,dna) ; found_of(X,fingerprints).

-found_of(alice,_).


/*
?- true:exonerated(X).
X = alice.

?- true:investigated(X).
X = bob.


*/

