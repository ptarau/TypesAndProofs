%--------------------------------------------------------------------------
% File     : SYJ208+1.009 : ILTP v1.1.2
% Domain   : Intuitionistic Syntactic
% Problem  : Cook pigeon-hole problem
% Version  : Especial.
%            Problem formulation : Prop. Non-Clausal. Inuit. Invalid.   Size 9
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
% Names    : ph_n9 : Dyckhoff's benchmark formulas (1997)
%
% Status (intuit.) : Non-Theorem
% Rating (intuit.) : 0.75 v1.1.0, 0.50 v1.0.0
%

% Comments : 
%--------------------------------------------------------------------------
fof(axiom1,axiom,(
( o11 v ( o12 v ( o13 v ( o14 v ( o15 v ( o16 v ( o17 v ( o18 v ~(~(o19)) ) ) ) ) ) ) ) ))).

fof(axiom2,axiom,(
( o21 v ( o22 v ( o23 v ( o24 v ( o25 v ( o26 v ( o27 v ( o28 v ~(~(o29)) ) ) ) ) ) ) ) ))).

fof(axiom3,axiom,(
( o31 v ( o32 v ( o33 v ( o34 v ( o35 v ( o36 v ( o37 v ( o38 v ~(~(o39)) ) ) ) ) ) ) ) ))).

fof(axiom4,axiom,(
( o41 v ( o42 v ( o43 v ( o44 v ( o45 v ( o46 v ( o47 v ( o48 v ~(~(o49)) ) ) ) ) ) ) ) ))).

fof(axiom5,axiom,(
( o51 v ( o52 v ( o53 v ( o54 v ( o55 v ( o56 v ( o57 v ( o58 v ~(~(o59)) ) ) ) ) ) ) ) ))).

fof(axiom6,axiom,(
( o61 v ( o62 v ( o63 v ( o64 v ( o65 v ( o66 v ( o67 v ( o68 v ~(~(o69)) ) ) ) ) ) ) ) ))).

fof(axiom7,axiom,(
( o71 v ( o72 v ( o73 v ( o74 v ( o75 v ( o76 v ( o77 v ( o78 v ~(~(o79)) ) ) ) ) ) ) ) ))).

fof(axiom8,axiom,(
( o81 v ( o82 v ( o83 v ( o84 v ( o85 v ( o86 v ( o87 v ( o88 v ~(~(o89)) ) ) ) ) ) ) ) ))).

fof(axiom9,axiom,(
( o91 v ( o92 v ( o93 v ( o94 v ( o95 v ( o96 v ( o97 v ( o98 v ~(~(o99)) ) ) ) ) ) ) ) ))).

fof(axiom10,axiom,(
( o101 v ( o102 v ( o103 v ( o104 v ( o105 v ( o106 v ( o107 v ( o108 v ~(~(o109)) ) ) ) ) ) ) ) ))).

fof(con,conjecture,(
( ( o11 & o21 ) v ( ( o11 & o31 ) v ( ( o11 & o41 ) v ( ( o11 & o51 ) v ( ( o11 & o61 ) v ( ( o11 & o71 ) v ( ( o11 & o81 ) v ( ( o11 & o91 ) v ( ( o11 & o101 ) v ( ( o21 & o31 ) v ( ( o21 & o41 ) v ( ( o21 & o51 ) v ( ( o21 & o61 ) v ( ( o21 & o71 ) v ( ( o21 & o81 ) v ( ( o21 & o91 ) v ( ( o21 & o101 ) v ( ( o31 & o41 ) v ( ( o31 & o51 ) v ( ( o31 & o61 ) v ( ( o31 & o71 ) v ( ( o31 & o81 ) v ( ( o31 & o91 ) v ( ( o31 & o101 ) v ( ( o41 & o51 ) v ( ( o41 & o61 ) v ( ( o41 & o71 ) v ( ( o41 & o81 ) v ( ( o41 & o91 ) v ( ( o41 & o101 ) v ( ( o51 & o61 ) v ( ( o51 & o71 ) v ( ( o51 & o81 ) v ( ( o51 & o91 ) v ( ( o51 & o101 ) v ( ( o61 & o71 ) v ( ( o61 & o81 ) v ( ( o61 & o91 ) v ( ( o61 & o101 ) v ( ( o71 & o81 ) v ( ( o71 & o91 ) v ( ( o71 & o101 ) v ( ( o81 & o91 ) v ( ( o81 & o101 ) v ( ( o91 & o101 ) v ( ( o12 & o22 ) v ( ( o12 & o32 ) v ( ( o12 & o42 ) v ( ( o12 & o52 ) v ( ( o12 & o62 ) v ( ( o12 & o72 ) v ( ( o12 & o82 ) v ( ( o12 & o92 ) v ( ( o12 & o102 ) v ( ( o22 & o32 ) v ( ( o22 & o42 ) v ( ( o22 & o52 ) v ( ( o22 & o62 ) v ( ( o22 & o72 ) v ( ( o22 & o82 ) v ( ( o22 & o92 ) v ( ( o22 & o102 ) v ( ( o32 & o42 ) v ( ( o32 & o52 ) v ( ( o32 & o62 ) v ( ( o32 & o72 ) v ( ( o32 & o82 ) v ( ( o32 & o92 ) v ( ( o32 & o102 ) v ( ( o42 & o52 ) v ( ( o42 & o62 ) v ( ( o42 & o72 ) v ( ( o42 & o82 ) v ( ( o42 & o92 ) v ( ( o42 & o102 ) v ( ( o52 & o62 ) v ( ( o52 & o72 ) v ( ( o52 & o82 ) v ( ( o52 & o92 ) v ( ( o52 & o102 ) v ( ( o62 & o72 ) v ( ( o62 & o82 ) v ( ( o62 & o92 ) v ( ( o62 & o102 ) v ( ( o72 & o82 ) v ( ( o72 & o92 ) v ( ( o72 & o102 ) v ( ( o82 & o92 ) v ( ( o82 & o102 ) v ( ( o92 & o102 ) v ( ( o13 & o23 ) v ( ( o13 & o33 ) v ( ( o13 & o43 ) v ( ( o13 & o53 ) v ( ( o13 & o63 ) v ( ( o13 & o73 ) v ( ( o13 & o83 ) v ( ( o13 & o93 ) v ( ( o13 & o103 ) v ( ( o23 & o33 ) v ( ( o23 & o43 ) v ( ( o23 & o53 ) v ( ( o23 & o63 ) v ( ( o23 & o73 ) v ( ( o23 & o83 ) v ( ( o23 & o93 ) v ( ( o23 & o103 ) v ( ( o33 & o43 ) v ( ( o33 & o53 ) v ( ( o33 & o63 ) v ( ( o33 & o73 ) v ( ( o33 & o83 ) v ( ( o33 & o93 ) v ( ( o33 & o103 ) v ( ( o43 & o53 ) v ( ( o43 & o63 ) v ( ( o43 & o73 ) v ( ( o43 & o83 ) v ( ( o43 & o93 ) v ( ( o43 & o103 ) v ( ( o53 & o63 ) v ( ( o53 & o73 ) v ( ( o53 & o83 ) v ( ( o53 & o93 ) v ( ( o53 & o103 ) v ( ( o63 & o73 ) v ( ( o63 & o83 ) v ( ( o63 & o93 ) v ( ( o63 & o103 ) v ( ( o73 & o83 ) v ( ( o73 & o93 ) v ( ( o73 & o103 ) v ( ( o83 & o93 ) v ( ( o83 & o103 ) v ( ( o93 & o103 ) v ( ( o14 & o24 ) v ( ( o14 & o34 ) v ( ( o14 & o44 ) v ( ( o14 & o54 ) v ( ( o14 & o64 ) v ( ( o14 & o74 ) v ( ( o14 & o84 ) v ( ( o14 & o94 ) v ( ( o14 & o104 ) v ( ( o24 & o34 ) v ( ( o24 & o44 ) v ( ( o24 & o54 ) v ( ( o24 & o64 ) v ( ( o24 & o74 ) v ( ( o24 & o84 ) v ( ( o24 & o94 ) v ( ( o24 & o104 ) v ( ( o34 & o44 ) v ( ( o34 & o54 ) v ( ( o34 & o64 ) v ( ( o34 & o74 ) v ( ( o34 & o84 ) v ( ( o34 & o94 ) v ( ( o34 & o104 ) v ( ( o44 & o54 ) v ( ( o44 & o64 ) v ( ( o44 & o74 ) v ( ( o44 & o84 ) v ( ( o44 & o94 ) v ( ( o44 & o104 ) v ( ( o54 & o64 ) v ( ( o54 & o74 ) v ( ( o54 & o84 ) v ( ( o54 & o94 ) v ( ( o54 & o104 ) v ( ( o64 & o74 ) v ( ( o64 & o84 ) v ( ( o64 & o94 ) v ( ( o64 & o104 ) v ( ( o74 & o84 ) v ( ( o74 & o94 ) v ( ( o74 & o104 ) v ( ( o84 & o94 ) v ( ( o84 & o104 ) v ( ( o94 & o104 ) v ( ( o15 & o25 ) v ( ( o15 & o35 ) v ( ( o15 & o45 ) v ( ( o15 & o55 ) v ( ( o15 & o65 ) v ( ( o15 & o75 ) v ( ( o15 & o85 ) v ( ( o15 & o95 ) v ( ( o15 & o105 ) v ( ( o25 & o35 ) v ( ( o25 & o45 ) v ( ( o25 & o55 ) v ( ( o25 & o65 ) v ( ( o25 & o75 ) v ( ( o25 & o85 ) v ( ( o25 & o95 ) v ( ( o25 & o105 ) v ( ( o35 & o45 ) v ( ( o35 & o55 ) v ( ( o35 & o65 ) v ( ( o35 & o75 ) v ( ( o35 & o85 ) v ( ( o35 & o95 ) v ( ( o35 & o105 ) v ( ( o45 & o55 ) v ( ( o45 & o65 ) v ( ( o45 & o75 ) v ( ( o45 & o85 ) v ( ( o45 & o95 ) v ( ( o45 & o105 ) v ( ( o55 & o65 ) v ( ( o55 & o75 ) v ( ( o55 & o85 ) v ( ( o55 & o95 ) v ( ( o55 & o105 ) v ( ( o65 & o75 ) v ( ( o65 & o85 ) v ( ( o65 & o95 ) v ( ( o65 & o105 ) v ( ( o75 & o85 ) v ( ( o75 & o95 ) v ( ( o75 & o105 ) v ( ( o85 & o95 ) v ( ( o85 & o105 ) v ( ( o95 & o105 ) v ( ( o16 & o26 ) v ( ( o16 & o36 ) v ( ( o16 & o46 ) v ( ( o16 & o56 ) v ( ( o16 & o66 ) v ( ( o16 & o76 ) v ( ( o16 & o86 ) v ( ( o16 & o96 ) v ( ( o16 & o106 ) v ( ( o26 & o36 ) v ( ( o26 & o46 ) v ( ( o26 & o56 ) v ( ( o26 & o66 ) v ( ( o26 & o76 ) v ( ( o26 & o86 ) v ( ( o26 & o96 ) v ( ( o26 & o106 ) v ( ( o36 & o46 ) v ( ( o36 & o56 ) v ( ( o36 & o66 ) v ( ( o36 & o76 ) v ( ( o36 & o86 ) v ( ( o36 & o96 ) v ( ( o36 & o106 ) v ( ( o46 & o56 ) v ( ( o46 & o66 ) v ( ( o46 & o76 ) v ( ( o46 & o86 ) v ( ( o46 & o96 ) v ( ( o46 & o106 ) v ( ( o56 & o66 ) v ( ( o56 & o76 ) v ( ( o56 & o86 ) v ( ( o56 & o96 ) v ( ( o56 & o106 ) v ( ( o66 & o76 ) v ( ( o66 & o86 ) v ( ( o66 & o96 ) v ( ( o66 & o106 ) v ( ( o76 & o86 ) v ( ( o76 & o96 ) v ( ( o76 & o106 ) v ( ( o86 & o96 ) v ( ( o86 & o106 ) v ( ( o96 & o106 ) v ( ( o17 & o27 ) v ( ( o17 & o37 ) v ( ( o17 & o47 ) v ( ( o17 & o57 ) v ( ( o17 & o67 ) v ( ( o17 & o77 ) v ( ( o17 & o87 ) v ( ( o17 & o97 ) v ( ( o17 & o107 ) v ( ( o27 & o37 ) v ( ( o27 & o47 ) v ( ( o27 & o57 ) v ( ( o27 & o67 ) v ( ( o27 & o77 ) v ( ( o27 & o87 ) v ( ( o27 & o97 ) v ( ( o27 & o107 ) v ( ( o37 & o47 ) v ( ( o37 & o57 ) v ( ( o37 & o67 ) v ( ( o37 & o77 ) v ( ( o37 & o87 ) v ( ( o37 & o97 ) v ( ( o37 & o107 ) v ( ( o47 & o57 ) v ( ( o47 & o67 ) v ( ( o47 & o77 ) v ( ( o47 & o87 ) v ( ( o47 & o97 ) v ( ( o47 & o107 ) v ( ( o57 & o67 ) v ( ( o57 & o77 ) v ( ( o57 & o87 ) v ( ( o57 & o97 ) v ( ( o57 & o107 ) v ( ( o67 & o77 ) v ( ( o67 & o87 ) v ( ( o67 & o97 ) v ( ( o67 & o107 ) v ( ( o77 & o87 ) v ( ( o77 & o97 ) v ( ( o77 & o107 ) v ( ( o87 & o97 ) v ( ( o87 & o107 ) v ( ( o97 & o107 ) v ( ( o18 & o28 ) v ( ( o18 & o38 ) v ( ( o18 & o48 ) v ( ( o18 & o58 ) v ( ( o18 & o68 ) v ( ( o18 & o78 ) v ( ( o18 & o88 ) v ( ( o18 & o98 ) v ( ( o18 & o108 ) v ( ( o28 & o38 ) v ( ( o28 & o48 ) v ( ( o28 & o58 ) v ( ( o28 & o68 ) v ( ( o28 & o78 ) v ( ( o28 & o88 ) v ( ( o28 & o98 ) v ( ( o28 & o108 ) v ( ( o38 & o48 ) v ( ( o38 & o58 ) v ( ( o38 & o68 ) v ( ( o38 & o78 ) v ( ( o38 & o88 ) v ( ( o38 & o98 ) v ( ( o38 & o108 ) v ( ( o48 & o58 ) v ( ( o48 & o68 ) v ( ( o48 & o78 ) v ( ( o48 & o88 ) v ( ( o48 & o98 ) v ( ( o48 & o108 ) v ( ( o58 & o68 ) v ( ( o58 & o78 ) v ( ( o58 & o88 ) v ( ( o58 & o98 ) v ( ( o58 & o108 ) v ( ( o68 & o78 ) v ( ( o68 & o88 ) v ( ( o68 & o98 ) v ( ( o68 & o108 ) v ( ( o78 & o88 ) v ( ( o78 & o98 ) v ( ( o78 & o108 ) v ( ( o88 & o98 ) v ( ( o88 & o108 ) v ( ( o98 & o108 ) v ( ( o19 & o29 ) v ( ( o19 & o39 ) v ( ( o19 & o49 ) v ( ( o19 & o59 ) v ( ( o19 & o69 ) v ( ( o19 & o79 ) v ( ( o19 & o89 ) v ( ( o19 & o99 ) v ( ( o19 & o109 ) v ( ( o29 & o39 ) v ( ( o29 & o49 ) v ( ( o29 & o59 ) v ( ( o29 & o69 ) v ( ( o29 & o79 ) v ( ( o29 & o89 ) v ( ( o29 & o99 ) v ( ( o29 & o109 ) v ( ( o39 & o49 ) v ( ( o39 & o59 ) v ( ( o39 & o69 ) v ( ( o39 & o79 ) v ( ( o39 & o89 ) v ( ( o39 & o99 ) v ( ( o39 & o109 ) v ( ( o49 & o59 ) v ( ( o49 & o69 ) v ( ( o49 & o79 ) v ( ( o49 & o89 ) v ( ( o49 & o99 ) v ( ( o49 & o109 ) v ( ( o59 & o69 ) v ( ( o59 & o79 ) v ( ( o59 & o89 ) v ( ( o59 & o99 ) v ( ( o59 & o109 ) v ( ( o69 & o79 ) v ( ( o69 & o89 ) v ( ( o69 & o99 ) v ( ( o69 & o109 ) v ( ( o79 & o89 ) v ( ( o79 & o99 ) v ( ( o79 & o109 ) v ( ( o89 & o99 ) v ( ( o89 & o109 ) v ( o99 & o109 ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
)).

%--------------------------------------------------------------------------
