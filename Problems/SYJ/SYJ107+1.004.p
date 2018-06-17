%--------------------------------------------------------------------------
% File     : SYJ107+1.004 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : 
% Version  : Especial.
%            Problem formulation : Intuit. Valid  Size 4
% English  : 

% Refs     : [SN00]  S. Schmitt & A. Nogin: test module "jprover_tests.ml",
%                    test formulas for JProver in MetaPRL, at
%                    http://cvs.metaprl.org:12000/cvsweb/metaprl/theories/
%                         itt/jprover_tests.ml
%            [ES99]  U. Egly & S. Schmitt. On intuitionistic proof
%                    transformations, their complexity, and application to 
%                    constructive program synthesis. Fundamenta 
%                    Informaticae, Special Issue: Symbolic Computation and 
%                    Artificial Intelligence, vol. 39, 1/2, p. 59-83, 1999
% Source   : [SN00]
% Names    : prop_n4 : JProver test formulae (2000)
%
% Status (intuit.) : Theorem
% Rating (intuit.) : 0.00 v1.0.0
%

% Comments : 
%--------------------------------------------------------------------------
fof(axiom1,axiom,(
a4)).

fof(axiom2,axiom,(
( b2 => ( ( b3 | a3 ) | b3 )) )).

fof(axiom3,axiom,(
( b1 => ( ( b2 | a2 ) | b2 )) )).

fof(axiom4,axiom,(
( b => ( ( b1 | a1 ) | b1 )) )).

fof(axiom5,axiom,(
( ( b | a ) | b ))).

fof(con,conjecture,(
( a | ( ( b & a1 ) | ( ( b1 & a2 ) | ( ( b2 & a3 ) | ( b3 & a4 ) ) ) ) )
)).

%--------------------------------------------------------------------------
