%--------------------------------------------------------------------------
% File     : SYN392+1 : ILTP v1.1.2
% Domain   : Syntactic
% Problem  : Pelletier 14
% Version  : Especial.
%            Theorem formulation : 2 propositions.
% English  : 

% Refs     : [Pel86] Pelletier (1986), Seventy-five Problems for Testing Au
%          : [Hah94] Haehnle (1994), Email to G. Sutcliffe
% Source   : [Hah94]
% Names    : Pelletier 14 [Pel86]

% Status   : Theorem
% Rating   : 0.11 v3.1.0, 0.17 v2.7.0, 0.00 v2.1.0
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.00 v1.0.0
%
% Syntax   : Number of formulae    :    1 (   0 unit)
%            Number of atoms       :    6 (   0 equality)
%            Maximal formula depth :    5 (   5 average)
%            Number of connectives :    7 (   2 ~  ;   2  v;   1  &)
%                                         (   2 <->;   0 ->;   0 <-)
%                                         (   0 <~>;   0 ~v;   0 ~&)
%            Number of predicates  :    2 (   2 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton;   0 !;   0 ?)
%            Maximal term depth    :    0 (   0 average)

% Comments : The clausal form of this problem is the same as SYN391+1.p.
%--------------------------------------------------------------------------
fof(pel14,conjecture,
    ( ( p1
    <-> p2 )
  <-> ( ( p2
        v ~ p1 )
      & ( ~ p2
        v p1 ) ) )).

%--------------------------------------------------------------------------
