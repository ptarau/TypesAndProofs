%--------------------------------------------------------------------------
% File     : SYJ201+1.001 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : de Bruijn's example
% Version  : Especial.
%            Problem formulation : Intuit. Valid  Size 1
% English  : LHS(2*N+1) => RHS(2*N+1) with

%            RHS(m) = &&_{i=1..m} p(i),
%            LHS(m) = &&_{i=1..m} ((p(i)<=>p(i+1)) => c(N))
%            where addition is computed modulo m, and with
%            c(N) = &&_{i=1..N} p(i)

% Refs     : [Dyc97] Roy Dyckhoff. Some benchmark formulas for
%                    intuitionistic propositional logic. At
%                    http://www.dcs.st-and.ac.uk/~rd/logic/marks.html
%          :         "de Bruijn, N.: personal communication in about 1990."
% Source   : [Dyc97]
% Names    : debruijn_p1 : Dyckhoff's benchmark formulas (1997)
%
% Status (intuit.) : Theorem
% Rating (intuit.) : 0.00 v1.0.0
%

% Comments : "quite a tough exercise for students to prove by natural
%             deduction" [Dyc97]
%--------------------------------------------------------------------------
fof(axiom1,axiom,(
( ( p1 <=> p2)  => ( p1 & ( p2 & p3 ) )) )).

fof(axiom2,axiom,(
( ( p2 <=> p3)  => ( p1 & ( p2 & p3 ) )) )).

fof(axiom3,axiom,(
( ( p3 <=> p1)  => ( p1 & ( p2 & p3 ) )) )).

fof(con,conjecture,(
( p1 & ( p2 & p3 ) )
)).

%--------------------------------------------------------------------------
