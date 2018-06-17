%--------------------------------------------------------------------------
% File     : SYN916+1 : ILTP v1.1.2
% Domain   : Syntactic
% Problem  : FALSE
% Version  : Biased.
% English  : 

% Refs     : 
% Source   : [TPTP]
% Names    :

% Status   : CounterSatisfiable
% Rating   : 0.00 v3.1.0
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.00 v1.1.0
%
% Syntax   : Number of formulae    :    1 (   1 unit)
%            Number of atoms       :    1 (   0 equality)
%            Maximal formula depth :    1 (   1 average)
%            Number of connectives :    0 (   0 ~  ;   0  |;   0  &)
%                                         (   0 <=>;   0 =>;   0 <=)
%                                         (   0 <~>;   0 ~|;   0 ~&)
%            Number of predicates  :    1 (   1 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton;   0 !;   0 ?)
%            Maximal term depth    :    0 (   0 average)

% Comments : Biased because it has $false which some systems don't understand
%            yet.
%--------------------------------------------------------------------------
fof(falsity,conjecture,
    ( $false )).
%--------------------------------------------------------------------------
