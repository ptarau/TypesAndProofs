%--------------------------------------------------------------------------
% File     : SYJ203+1.007 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : Formulae requiring many contractions
% Version  : Especial.
%            Problem formulation : Intuit. Valid  Size 7
% English  : (((&&_{i=1..N} p(i)) | (||_{i=1..N} (p(i)=>f)))=>f)=>f

% Refs     : [Dyc97] Roy Dyckhoff. Some benchmark formulas for
%                    intuitionistic propositional logic. At
%                    http://www.dcs.st-and.ac.uk/~rd/logic/marks.html
%          : [Fr88]  T. Franzen, Algorithmic Aspects of intuitionistic
%                    propositional logic, SICS Research Report R87010B,
%                    1988.
%          : [Fr89]  T. Franzen, Algorithmic Aspects of intuitionistic
%                    propositional logic II, SICS Research Report
%                    R-89/89006, 1989.
% Source   : [Dyc97]
% Names    : con_p7 : Dyckhoff's benchmark formulas (1997)
%
% Status (intuit.) : Theorem
% Rating (intuit.) : 0.00 v1.0.0
%

% Comments : "proof in LJ needs n contractions" [Dyc97]
%--------------------------------------------------------------------------
fof(axiom1,axiom,(
( ( ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & p7 ) ) ) ) ) ) | ( ( p1 => f)  | ( ( p2 => f)  | ( ( p3 => f)  | ( ( p4 => f)  | ( ( p5 => f)  | ( ( p6 => f)  | ( p7 => f)  ) ) ) ) ) ) ) => f) )).

fof(con,conjecture,(
f
)).

%--------------------------------------------------------------------------
