%--------------------------------------------------------------------------
% File     : SYN416+1 : ILTP v1.1.2
% Domain   : Syntactic
% Problem  : Pelletier Problem 16
% Version  : Especial.
% English  : 16:A surprising theorem of propositional logic.

% Refs     : [Pel86] Pelletier (1986), Seventy-five Problems for Testing Au
%          : [Hah94] Haehnle (1994), Email to G. Sutcliffe
% Source   : [Hah94]
% Names    : Pelletier 16 [Pel86]

% Status   : Theorem
% Rating   : 0.11 v3.1.0, 0.00 v2.5.0, 0.33 v2.4.0, 0.33 v2.2.1, 0.00 v2.1.0
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.00 v1.0.0
%
% Syntax   : Number of formulae    :    1 (   0 unit)
%            Number of atoms       :    4 (   0 equality)
%            Maximal formula depth :    3 (   3 average)
%            Number of connectives :    3 (   0 ~  ;   1  v;   0  &)
%                                         (   0 <->;   2 ->;   0 <-)
%                                         (   0 <~>;   0 ~v;   0 ~&)
%            Number of predicates  :    2 (   2 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton;   0 !;   0 ?)
%            Maximal term depth    :    0 (   0 average)

% Comments : 
%--------------------------------------------------------------------------
fof(pel16,conjecture,
    ( ( p
     -> q )
    v ( q
     -> p ) )).

%--------------------------------------------------------------------------
