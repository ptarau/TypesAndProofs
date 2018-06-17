%--------------------------------------------------------------------------
% File     : LCL230+1 : ILTP v1.1.2
% Domain   : Logic Calculi (Propositional)
% Problem  : Principia Mathematica 2.85
% Version  : Especial.
% English  : Judged by [SRM73] to be the 'hardest' of the
%            first 67 theorems of [WR27].

% Refs     : [WR27]  Whitehead & Russell (1927), Principia Mathematica
%          : [SRM73] Siklossy et al. (1973), Breadth First Search: Some Sur
%          : [Pel86] Pelletier (1986), Seventy-five Problems for Testing Au
%          : [Hah94] Haehnle (1994), Email to G. Sutcliffe
% Source   : [Hah94]
% Names    : Pelletier 5 [Pel86]

% Status   : Theorem
% Rating   : 0.11 v3.1.0, 0.00 v2.5.0, 0.33 v2.4.0, 0.33 v2.2.1, 0.00 v2.1.0
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.00 v1.0.0
%
% Syntax   : Number of formulae    :    1 (   0 unit)
%            Number of atoms       :    7 (   0 equality)
%            Maximal formula depth :    4 (   4 average)
%            Number of connectives :    6 (   0 ~  ;   3  |;   0  &)
%                                         (   0 <=>;   3 =>;   0 <=)
%                                         (   0 <~>;   0 ~|;   0 ~&)
%            Number of predicates  :    3 (   3 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton;   0 !;   0 ?)
%            Maximal term depth    :    0 (   0 average)

% Comments : 
%--------------------------------------------------------------------------
fof(pel5,conjecture,
    ( ( ( p
        | q )
     => ( p
        | r ) )
   => ( p
      | ( q
       => r ) ) )).

%--------------------------------------------------------------------------
