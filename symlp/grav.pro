:-include('compile_clauses.pro').

'Negative gravity fields are possible' =>
    'Planets and stars would disperse' ;
    'Atmospheres of planets would be pushed away from their surfaces' ;
    'Unresolvable paradoxes in physics'.

'Planets and stars would disperse' => false.

'Unresolvable paradoxes in physics' =>
   'Relativity theory is incorrect' ;
   'Quantum field theory is incorrect'.

'Relativity theory is incorrect' => false.
'Quantum field theory is incorrect' => false.

'Atmospheres of planets would be pushed away from their surfaces' => false.

/*
?-false:'Negative gravity fields are possible'.
true
*/
