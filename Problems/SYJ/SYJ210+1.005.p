%--------------------------------------------------------------------------
% File     : SYJ210+1.005 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : Formulae with normal natural deduction proofs only of exponential size
% Version  : Especial.
%            Problem formulation : Inuit. Invalid.   Size 5
% English  : (~~p(N) & &&_{i=1..N} (p(i) => p(i) => p(i-1))) => p(0) 

% Refs     : [Dyc97] Roy Dyckhoff. Some benchmark formulas for
%                    intuitionistic propositional logic. At
%                    http://www.dcs.st-and.ac.uk/~rd/logic/marks.html
%          : [Sch97] H. Schwichtenberg, Termination of permutative
%                    conversions in Gentzen's sequent calculus,
%                    unpublished (1997). 
% Source   : [Dyc97]
% Names    : schwicht_n5 : Dyckhoff's benchmark formulas (1997)
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.00 v1.0.0
%

% Comments : "...no normal natural deduction proof of size less than an
%             expontial function of N.
%            ..Our experience of these problems is that they can be decided
%            very fast but can generate space problems, e.g. for some
%            implementations of Prolog." [Dyc97]
%--------------------------------------------------------------------------
fof(axiom1,axiom,(
~(~(p5)))).

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

fof(con,conjecture,(
p0
)).

%--------------------------------------------------------------------------
