%--------------------------------------------------------------------------
% File     : SYN387+1 : ILTP v1.1.2
% Domain   : Syntactic
% Problem  : The Law of Excluded Middle
% Version  : Especial.
%            Theorem formulation : 1 proposition.
% English  : The Law of Excluded Middle: can be quite difficult for
%            'natural' systems.

% Refs     : [NS72]  Newell & Simon (1972), Human Problem Solving
%          : [Pel86] Pelletier (1986), Seventy-five Problems for Testing Au
%          : [Hah94] Haehnle (1994), Email to G. Sutcliffe
% Source   : [Hah94]
% Names    : Pelletier 6 [Pel86]

% Status   : Theorem
% Rating   : 0.00 v2.1.0
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.00 v1.0.0
%
% Syntax   : Number of formulae    :    1 (   0 unit)
%            Number of atoms       :    2 (   0 equality)
%            Maximal formula depth :    3 (   3 average)
%            Number of connectives :    2 (   1 ~  ;   1  v;   0  &)
%                                         (   0 <->;   0 ->;   0 <-)
%                                         (   0 <~>;   0 ~v;   0 ~&)
%            Number of predicates  :    1 (   1 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton;   0 !;   0 ?)
%            Maximal term depth    :    0 (   0 average)

% Comments : The clausal form of this problem is the same as SYN001+1.p.
%--------------------------------------------------------------------------
fof(pel6,conjecture,
    ( p
    v ~ p )).

%--------------------------------------------------------------------------
