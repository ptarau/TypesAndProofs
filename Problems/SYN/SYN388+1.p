%--------------------------------------------------------------------------
% File     : SYN388+1 : ILTP v1.1.2
% Domain   : Syntactic
% Problem  : Expanded Law of Excluded Middle
% Version  : Especial.
%            Theorem formulation : 1 proposition.
% English  : Expanded Law of Excluded Middle. The strategies of the
%            original Logic Theorist cannot prove this.

% Refs     : [NS72]  Newell & Simon (1972), Human Problem Solving
%          : [Pel86] Pelletier (1986), Seventy-five Problems for Testing Au
%          : [Hah94] Haehnle (1994), Email to G. Sutcliffe
% Source   : [Hah94]
% Names    : Pelletier 7 [Pel86]

% Status   : Theorem
% Rating   : 0.00 v2.1.0
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.00 v1.0.0
%
% Syntax   : Number of formulae    :    1 (   0 unit)
%            Number of atoms       :    2 (   0 equality)
%            Maximal formula depth :    5 (   5 average)
%            Number of connectives :    4 (   3 ~  ;   1  |;   0  &)
%                                         (   0 <=>;   0 =>;   0 <=)
%                                         (   0 <~>;   0 ~|;   0 ~&)
%            Number of predicates  :    1 (   1 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton;   0 !;   0 ?)
%            Maximal term depth    :    0 (   0 average)

% Comments : The clausal form of this problem is the same as SYN001+1.p.
%--------------------------------------------------------------------------
fof(pel7,conjecture,
    ( p
    | ~ ~ ~ p )).

%--------------------------------------------------------------------------
