:-include('compile_clauses.pro').

p => q ; r.
q => r ; s.
r => false.
s => false.

/*
?- false:p.
true.
*/