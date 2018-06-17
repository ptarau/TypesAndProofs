%--------------------------------------------------------------------------
% File     : SYJ208+1.002 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : Cook pigeon-hole problem
% Version  : Especial.
%            Problem formulation : Prop. Non-Clausal. Inuit. Invalid.   Size 2
% English  : Suppose there are N holes and N+1 pigeons to put in the
%            holes. Every pigeon is in a hole and no hole contains more
%            than one pigeon. Prove that this is impossible. The size is
%            the number of pigeons.
%            LHS(N) -> RHS(N) with 
%            LHS(N) - &&_{p-1..N+1} (vv_{h-1,..N-1} o(p,h) v ~~o(p,N) )
%            RHS(N) - vv_{h-1..N, p1-1..{N+1}, p2-{p1+1}..{N+1}} s(p1,p2,h)
%            with s(p1,p2,h) - o(p1,h) & o(p2,h)

% Refs     : [Dyc97] Roy Dyckhoff. Some benchmark formulas for
%                    intuitionistic propositional logic. At
%                    http://www.dcs.st-and.ac.uk/~rd/logic/marks.html
%          : [CR79]  Cook & Reckhow (1979), The Relative Efficiency of
%                    Propositional Proof Systems, Journal of Symbolic
%                    Logic 44, pp.36-50.

%          : [Pel86] Pelletier (1986), Seventy-five Problems for Testing
%                    Automatic Theorem Provers, Journal of Automated
%                    Reasoning 2(2), pp.191-216.
% Source   : [Dyc97]
% Names    : ph_n2 : Dyckhoff's benchmark formulas (1997)
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.00 v1.0.0
%

% Comments : 
%--------------------------------------------------------------------------
fof(axiom1,axiom,(
( o11 v ~(~(o12)) ))).

fof(axiom2,axiom,(
( o21 v ~(~(o22)) ))).

fof(axiom3,axiom,(
( o31 v ~(~(o32)) ))).

fof(con,conjecture,(
( ( o11 & o21 ) v ( ( o11 & o31 ) v ( ( o21 & o31 ) v ( ( o12 & o22 ) v ( ( o12 & o32 ) v ( o22 & o32 ) ) ) ) ) )
)).

%--------------------------------------------------------------------------
