%--------------------------------------------------------------------------
% File     : SYJ204+1.015 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : Formulae with normal natural deduction proofs only of exponential size
% Version  : Especial.
%            Problem formulation : Intuit. Valid  Size 15
% English  : (p(N) & &&_{i=1..N} (p(i) => p(i) => p(i-1))) => p(0)

% Refs     : [Dyc97] Roy Dyckhoff. Some benchmark formulas for
%                    intuitionistic propositional logic. At
%                    http://www.dcs.st-and.ac.uk/~rd/logic/marks.html
%          : [Sch97] H. Schwichtenberg, Termination of permutative
%                    conversions in Gentzen's sequent calculus,
%                    unpublished (1997). 
% Source   : [Dyc97]
% Names    : schwicht_p15 : Dyckhoff's benchmark formulas (1997)
%
% Status (intuit.) : Theorem
% Rating (intuit.) : 0.00 v1.0.0
%

% Comments : "...no normal natural deduction proof of size less than an
%             expontial function of N.
%            ..Our experience of these problems is that they can be decided
%            very fast but can generate space problems, e.g. for some
%            implementations of Prolog." [Dyc97]
%--------------------------------------------------------------------------
fof(axiom1,axiom,(
p15)).

fof(axiom2,axiom,(
( p1 => ( p1 => p0) ) )).

fof(axiom3,axiom,(
( p2 => ( p2 => p1) ) )).

fof(axiom4,axiom,(
( p3 => ( p3 => p2) ) )).

fof(axiom5,axiom,(
( p4 => ( p4 => p3) ) )).

fof(axiom6,axiom,(
( p5 => ( p5 => p4) ) )).

fof(axiom7,axiom,(
( p6 => ( p6 => p5) ) )).

fof(axiom8,axiom,(
( p7 => ( p7 => p6) ) )).

fof(axiom9,axiom,(
( p8 => ( p8 => p7) ) )).

fof(axiom10,axiom,(
( p9 => ( p9 => p8) ) )).

fof(axiom11,axiom,(
( p10 => ( p10 => p9) ) )).

fof(axiom12,axiom,(
( p11 => ( p11 => p10) ) )).

fof(axiom13,axiom,(
( p12 => ( p12 => p11) ) )).

fof(axiom14,axiom,(
( p13 => ( p13 => p12) ) )).

fof(axiom15,axiom,(
( p14 => ( p14 => p13) ) )).

fof(axiom16,axiom,(
( p15 => ( p15 => p14) ) )).

fof(con,conjecture,(
p0
)).

%--------------------------------------------------------------------------
