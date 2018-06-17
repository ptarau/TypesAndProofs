%--------------------------------------------------------------------------
% File     : SYJ205+1.015 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : Formulae of Korn & Kreitz
% Version  : Especial.
%            Problem formulation : Intuit. Valid  Size 15
% English  : ((A & B(N) & C1(N)) => f) & ((C2(N) & B(N) & A) => f) with
%            A = (a(0) => f), B(N) = (b(N) => b(0) => a(N)),
%            C1(N) = (&&_{i=1..n} ((b(i-1) => a(i)) => a(i-1))),
%            C2(N) = (&&_{i=n..1} ((b(i-1) => a(i)) => a(i-1)))

% Refs     : [Dyc97] Roy Dyckhoff. Some benchmark formulas for
%                    intuitionistic propositional logic. At
%                    http://www.dcs.st-and.ac.uk/~rd/logic/marks.html
%          : [KK97]  D. Korn & C. Kreitz, A constructively adequate
%                    refutation system for intuitionistic logic,
%                    position paper at Tableaux'97, available at
%                    http://www.cs.uni-potsdam.de/ti/kreitz/PDF/
% Source   : [Dyc97]
% Names    : kk_p15 : Dyckhoff's benchmark formulas (1997)
%
% Status (intuit.) : Theorem
% Rating (intuit.) : 0.50 v1.1.0, 1.00 v1.0.0
%

% Comments : 
%--------------------------------------------------------------------------
fof(con,conjecture,(
( ( ( ( a0 => f)  & ( ( ( b15 => b0)  => a15)  & ( ( ( b0 => a1)  => a0)  & ( ( ( b1 => a2)  => a1)  & ( ( ( b2 => a3)  => a2)  & ( ( ( b3 => a4)  => a3)  & ( ( ( b4 => a5)  => a4)  & ( ( ( b5 => a6)  => a5)  & ( ( ( b6 => a7)  => a6)  & ( ( ( b7 => a8)  => a7)  & ( ( ( b8 => a9)  => a8)  & ( ( ( b9 => a10)  => a9)  & ( ( ( b10 => a11)  => a10)  & ( ( ( b11 => a12)  => a11)  & ( ( ( b12 => a13)  => a12)  & ( ( ( b13 => a14)  => a13)  & ( ( b14 => a15)  => a14)  ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) => f)  & ( ( ( ( b14 => a15)  => a14)  & ( ( ( b13 => a14)  => a13)  & ( ( ( b12 => a13)  => a12)  & ( ( ( b11 => a12)  => a11)  & ( ( ( b10 => a11)  => a10)  & ( ( ( b9 => a10)  => a9)  & ( ( ( b8 => a9)  => a8)  & ( ( ( b7 => a8)  => a7)  & ( ( ( b6 => a7)  => a6)  & ( ( ( b5 => a6)  => a5)  & ( ( ( b4 => a5)  => a4)  & ( ( ( b3 => a4)  => a3)  & ( ( ( b2 => a3)  => a2)  & ( ( ( b1 => a2)  => a1)  & ( ( ( b0 => a1)  => a0)  & ( ( ( b15 => b0)  => a15)  & ( a0 => f)  ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) => f)  )
)).

%--------------------------------------------------------------------------
