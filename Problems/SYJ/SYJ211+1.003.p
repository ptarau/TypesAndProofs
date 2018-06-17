%--------------------------------------------------------------------------
% File     : SYJ211+1.003 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : Formulae of Korn & Kreitz
% Version  : Especial.
%            Problem formulation : Inuit. Invalid.   Size 3
% English  : (A & B(N) & C(N)) => f with
%            A = (a(0) => f), B(N) = (~~b(N) => b(0) => a(N)),
%            C(N) = (&&_{i=1..n} ((~~b(i-1) => a(i)) => a(i-1))),

% Refs     : [Dyc97] Roy Dyckhoff. Some benchmark formulas for
%                    intuitionistic propositional logic. At
%                    http://www.dcs.st-and.ac.uk/~rd/logic/marks.html
%          : [KK97]  D. Korn & C. Kreitz, A constructively adequate
%                    refutation system for intuitionistic logic,
%                    position paper at Tableaux'97, available at
%                    http://www.cs.uni-potsdam.de/ti/kreitz/PDF/
% Source   : [Dyc97]
% Names    : kk_n3 : Dyckhoff's benchmark formulas (1997)
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.00 v1.0.0
%

% Comments : 
%--------------------------------------------------------------------------
fof(axiom1,axiom,(
( a0 => f) )).

fof(axiom2,axiom,(
( ( ~(~(b3)) => b0)  => a3) )).

fof(axiom3,axiom,(
( ( ~(~(b0)) => a1)  => a0) )).

fof(axiom4,axiom,(
( ( ~(~(b1)) => a2)  => a1) )).

fof(axiom5,axiom,(
( ( ~(~(b2)) => a3)  => a2) )).

fof(con,conjecture,(
f
)).

%--------------------------------------------------------------------------
