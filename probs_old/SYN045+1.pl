%--------------------------------------------------------------------------
% File     : SYN045+1 : ILTP v1.1.2
% Domain   : Syntactic
% Problem  : Pelletier Problem 13
% Version  : Especial.
% English  : 

% Refs     : [Pel86] Pelletier (1986), Seventy-five Problems for Testing Au
%          : [Hah94] Haehnle (1994), Email to G. Sutcliffe
% Source   : [Hah94]
% Names    : Pelletier 13 [Pel86]

% Status   : Theorem
% Rating   : 0.00 v2.1.0
%
% Status (intuit.) : Theorem
% Rating (intuit.) : 0.00 v1.0.0
%
% Syntax   : Number of formulae    :    1 (   0 unit)
%            Number of atoms       :    7 (   0 equality)
%            Maximal formula depth :    4 (   4 average)
%            Number of connectives :    6 (   0 ~  ;   3  v;   2  &)
%                                         (   1 <->;   0 ->;   0 <-)
%                                         (   0 <~>;   0 ~v;   0 ~&)
%            Number of predicates  :    3 (   3 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton;   0 !;   0 ?)
%            Maximal term depth    :    0 (   0 average)

% Comments : 
%--------------------------------------------------------------------------
fof(pel13,conjecture,
    ( ( p
      v ( q
        & r ) )
  <-> ( ( p
        v q )
      & ( p
        v r ) ) )).

%--------------------------------------------------------------------------
