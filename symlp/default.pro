:-include('compile_clauses.pro').

fly(X) <= bird(X),false:challanged_bird(X).

+bird(tweety).
+bird(chicken_little).
+bird(eagle_joe).
+bird(humming_jenny).

-challanged_bird(eagle_joe).
-challanged_bird(humming_jenny).

/*
?- true:fly(X).
X = eagle_joe ;
X = humming_jenny.
*/