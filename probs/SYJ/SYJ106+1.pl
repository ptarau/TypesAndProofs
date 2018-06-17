%--------------------------------------------------------------------------
% File     : SYJ106+1 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : 
% Version  : Especial.
% English  : 

% Refs     : [SN00]  S. Schmitt & A. Nogin: test module "jprover_tests.ml",
%                    test formulas for JProver in MetaPRL, at
%                    http://cvs.metaprl.org:12000/cvsweb/metaprl/theories/
%                         itt/jprover_tests.ml
%            [OK96] J. Otten und C. Kreitz. A uniform proof procedure
%                   for classical and non-classical logics. In KI-96: 
%                   Advances in Artificial Intelligence, LNAI 1137, 
%                   p. 307-319, Springer Verlag, 1996.
% Source   : [SN00]
% Names    : jens_prop : JProver test formulae (2000)
%
% Status (intuit.) : Theorem
% Rating (intuit.) : 0.00 v1.0.0
%

% Comments : 
%--------------------------------------------------------------------------
fof(axiom1,axiom,(
s)).

fof(axiom2,axiom,(
( ~(( t -> r) ) -> p) )).

fof(con,conjecture,(
( ~(( ( p -> q)  & ( t -> r)  )) -> ( ~(~(p)) & ( s & s ) )) 
)).

%--------------------------------------------------------------------------
