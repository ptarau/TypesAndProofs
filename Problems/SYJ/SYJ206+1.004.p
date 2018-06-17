%--------------------------------------------------------------------------
% File     : SYJ206+1.004 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : Equivalences
% Version  : Especial.
%            Problem formulation : Intuit. Valid  Size 4
% English  : (..(( p(1) <=> p(2)) <=> p(3)) <=> .. <=> p(N))  <=>
%            (p(N) <=> ( p(N-1) <=> (.. (p(2) <=> p(1)) ) ))

% Refs     : [Dyc97] Roy Dyckhoff. Some benchmark formulas for
%                    intuitionistic propositional logic. At
%                    http://www.dcs.st-and.ac.uk/~rd/logic/marks.html
% Source   : [Dyc97]
% Names    : equiv_p4 : Dyckhoff's benchmark formulas (1997)
%
% Status (intuit.) : Theorem
% Rating (intuit.) : 0.00 v1.0.0
%

% Comments : this formula is different to the equivalences formulated
%            in Pelletier 71 [Pel86], TPTP SYN007+1
%--------------------------------------------------------------------------
fof(con,conjecture,(
( ( ( ( a1 <=> a2)  <=> a3)  <=> a4)  <=> ( a4 <=> ( a3 <=> ( a2 <=> a1) ) ) ) 
)).

%--------------------------------------------------------------------------
