%--------------------------------------------------------------------------
% File     : SYJ107+1.001 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : 
% Version  : Especial.
%            Problem formulation : Intuit. Valid  Size 1
% English  : 

% Refs     : [SN00]  S. Schmitt & A. Nogin: test module "jprover_tests.ml",
%                    test formulas for JProver in MetaPRL, at
%                    http://cvs.metaprl.org:12000/cvsweb/metaprl/theories/
%                         itt/jprover_tests.ml
%            [ES99]  U. Egly & S. Schmitt. On intuitionistic proof
%                    transformations, their complexity, and application to 
%                    constructive program synthesis". Fundamenta 
%                    Informaticae, Special Issue: Symbolic Computation and 
%                    Artificial Intelligence, vol. 39, 1/2, p. 59-83, 1999
% Source   : [SN00]
% Names    : prop_n1 : JProver test formulae (2000)
%
% Status (intuit.) : Theorem
% Rating (intuit.) : 0.00 v1.0.0
%

% Comments : cause exponential proof length in EVERY LJ proof wrt. the 
%            proof length of a given LJmc proof in propositional 
%            intuitionistic logic 
%--------------------------------------------------------------------------
fof(axiom1,axiom,(
c)).

fof(axiom2,axiom,(
( ( b | a ) | b ))).

fof(con,conjecture,(
( a | ( b & c ) )
)).

%--------------------------------------------------------------------------
