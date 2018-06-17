%--------------------------------------------------------------------------
% File     : SYN047+1 : ILTP v1.1.2
% Domain   : Syntactic
% Problem  : Pelletier Problem 17
% Version  : Especial.
% English  : A problem which appears to not be provable by [BBH72].
%            For details of why not, see [Pel82] p.135f.

% Refs     : [BBH72] Bledsoe et al. (1972), Computer Proofs of Limit Theore
%          : [Pel82] Pelletier (1982), Completely Non-clausal, Completely H
%          : [Pel86] Pelletier (1986), Seventy-five Problems for Testing Au
%          : [Hah94] Haehnle (1994), Email to G. Sutcliffe
% Source   : [Hah94]
% Names    : Pelletier 17 [Pel86]

% Status   : Theorem
% Rating   : 0.11 v3.1.0, 0.00 v2.5.0, 0.33 v2.4.0, 0.33 v2.2.1, 0.00 v2.1.0
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.00 v1.0.0
%
% Syntax   : Number of formulae    :    1 (   0 unit)
%            Number of atoms       :   10 (   0 equality)
%            Maximal formula depth :    6 (   6 average)
%            Number of connectives :   12 (   3 ~  ;   4  v;   2  &)
%                                         (   1 <->;   2 ->;   0 <-)
%                                         (   0 <~>;   0 ~v;   0 ~&)
%            Number of predicates  :    4 (   4 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton;   0 !;   0 ?)
%            Maximal term depth    :    0 (   0 average)

% Comments : 
%--------------------------------------------------------------------------
fof(pel17,conjecture,
    ( ( ( p
        & ( q
         -> r ) )
     -> s )
  <-> ( ( ~ p
        v q
        v s )
      & ( ~ p
        v ~ r
        v s ) ) )).

%--------------------------------------------------------------------------
