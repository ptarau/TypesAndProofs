%--------------------------------------------------------------------------
% File     : SYJ207+1.006 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : de Bruijn's example
% Version  : Especial.
%            Problem formulation : Inuit. Invalid.   Size 6
% English  : LHS(2*N) -> (p0 | RHS(2*N) | ~p0)
%            RHS(m) = &&_{i=1..m} p(i),
%            LHS(m) = &&_{i=1..m} ((p(i)<=>p(i+1)) => c(N))
%            where addition is computed modulo m, and with
%            c(N) = &&_{i=1..N} p(i)

% Refs     : [Dyc97] Roy Dyckhoff. Some benchmark formulas for
%                    intuitionistic propositional logic. At
%                    http://www.dcs.st-and.ac.uk/~rd/logic/marks.html
%          :         "de Bruijn, N.: personal communication in about 1990."
% Source   : [Dyc97]
% Names    : debruijn_n6 : Dyckhoff's benchmark formulas (1997)
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.75 v1.1.0, 0.50 v1.0.0
%

% Comments : "quite a tough exercise for students to prove by natural
%             deduction" [Dyc97]
%--------------------------------------------------------------------------
fof(axiom1,axiom,(
( ( p1 <=> p2)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(axiom2,axiom,(
( ( p2 <=> p3)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(axiom3,axiom,(
( ( p3 <=> p4)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(axiom4,axiom,(
( ( p4 <=> p5)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(axiom5,axiom,(
( ( p5 <=> p6)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(axiom6,axiom,(
( ( p6 <=> p7)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(axiom7,axiom,(
( ( p7 <=> p8)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(axiom8,axiom,(
( ( p8 <=> p9)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(axiom9,axiom,(
( ( p9 <=> p10)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(axiom10,axiom,(
( ( p10 <=> p11)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(axiom11,axiom,(
( ( p11 <=> p12)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(axiom12,axiom,(
( ( p12 <=> p1)  => ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) )) )).

fof(con,conjecture,(
( p0 | ( ( p1 & ( p2 & ( p3 & ( p4 & ( p5 & ( p6 & ( p7 & ( p8 & ( p9 & ( p10 & ( p11 & p12 ) ) ) ) ) ) ) ) ) ) ) | ~(p0) ) )
)).

%--------------------------------------------------------------------------
