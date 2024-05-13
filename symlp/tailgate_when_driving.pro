:-include('compile_clauses.pro').

'tailgate when driving' =>
    'Increased accident risk';
    'Reduced reaction time'.
'Increased accident risk' =>
    'Higher insurance premiums',
    'Severe injury likelihood',
    'Vehicle damage costs',
    'Legal consequences',
    'Emotional trauma impact'.
'Reduced reaction time' =>
    'Increased accident risk',
    'Delayed braking response',
    'Higher collision likelihood',
    'Compromised driving safety'.
'Higher insurance premiums' => false.
'Severe injury likelihood' => false.
'Vehicle damage costs' => false.
'Legal consequences' => false.
'Emotional trauma impact' => false.
'Delayed braking response' => false.
'Higher collision likelihood' => false.
'Compromised driving safety' => false.

/*
?- false:'tailgate when driving'.
true ;
true.
*/
