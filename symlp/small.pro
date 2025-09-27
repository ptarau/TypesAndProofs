:-include('compile_clauses.pro').

p <= q,r, false:f.
q <= a,b.
q <= d.
r <= a,c.
r <= d.
d <= false:g,false:h.

+a.
+b.
+c.


f => g ; h.

-e1.
-e2.

g => e1.
h => e2.

