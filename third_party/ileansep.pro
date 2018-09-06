%% Version:  1.00   -   Date:  18/01/2005   -   File: ileansep_swi.pl
%%
%% Purpose: ileanSeP: An Intuitionistic Sequent Theorem Prover
%%
%% Author:  Jens Otten
%% Web:     www.leancop.de/ileansep/
%%
%% Usage:   sep_prove(F).   % where F is a first-order formula
%%                      %  e.g. F=(all X: ex Y:(~q,p(Y)=>p(X)))
%%
%% Copyright: (c) 2005 by Jens Otten
%% License:   GNU General Public License


:- op(1130, xfy, <=>). :- op(1110, xfy, =>). % :- op(500, fy, ~).
:- op( 500,  fy, all). :- op( 500,  fy, ex). :- op(500,xfy, :).

%%% prove formula


sep_prove(F) :- map_operators(F,G),sep_prove(G,1),!.

sep_prove_orig(F) :- 
 %Time1 is cputime, 
 sep_prove(F,1)
 %           ,Time2 is cputime, Time is Time2-Time1, print(Time)
 .

sep_prove(F,VLim) :- % display(VLim), nl, 
  sep_prove([],[F],l,[],VLim).
sep_prove(F,VLim) :- \+ sep_no_mul([([F],0)]), VLim1 is VLim+1, sep_prove(F,VLim1).

%%% check multiplicities

sep_no_mul([]).
sep_no_mul([([F],Pol)|T]) :- (F=(A,B);F=(A;B)) ->
                         !, sep_no_mul([([A],Pol),([B],Pol)|T]).
sep_no_mul([([F],Pol)|T]) :- sep_fml(F,Pol,_,L1,R1,L2,R2,_,_,V,U,U), !,
                         V==[], sep_no_mul([(L1,1),(R1,0),(L2,1),(R2,0)|T]).
sep_no_mul([_|T]):- sep_no_mul(T).

%%% specification of inference rules
% sep_fml(formula, polarity, invertible/noninvertible, add to left side
%     of 1st premise, right side of 1st premise, add to left side
%     of 2nd premise, right side of 2nd premise, position, free
%     variables, new free variable, term to be copied, copy of term)

sep_fml((A,B),  1,inv,[A,B],            [], [], [], _, _,[], [], [] ).
sep_fml((A,B),  0,inv,[],               [A],[], [B],_, _,[], [], [] ).
sep_fml((A;B),  1,inv,[A],              [], [B],[], _, _,[], [], [] ).
sep_fml((A;_),  0,nin,[],               [A],[], [], _, _,[], [], [] ).
sep_fml((_;B),  0,nin,[],               [B],[], [], _, _,[], [], [] ).
sep_fml((A=>B), 1,nin,[(A=>B)],         [C],[D],[], _, _,[!],A:B,C:D).
sep_fml((A=>B), 0,inv,[A],              [B],[], [], _, _,[], [], [] ).
sep_fml((A<=>B),1,inv,[((A=>B),(B=>A))],[], [], [], _, _,[], [], [] ).
sep_fml((A<=>B),0,inv,[], [((A=>B),(B=>A))],[], [], _, _,[], [], [] ).
sep_fml((~A),   1,nin,[~A],             [C],[], [], _, _,[!], A, C  ).
sep_fml((~A),   0,inv,[A],              [], [], [], _, _,[], [], [] ).
sep_fml(all X:A,1,nin,[C,all X:A],      [], [], [], _, _,[Y],X:A,Y:C).
sep_fml(all X:A,0,inv,[],               [C],[], [], S,FV,[],(X,A),(S^FV,C)).
sep_fml(ex X:A, 1,inv,[C],              [], [], [], S,FV,[],(X,A),(S^FV,C)).
sep_fml(ex X:A, 0,nin,[],               [C],[], [], _, _,[Y],X:A,Y:C).

%%% proof search
% sep_prove(left side, right side, position, free variables, variable limit)

sep_prove(Left,Right,S,FreeV,VarLim) :-
    ( (append(LeftA,[F|LeftB],Left), Pol=1, append(LeftA,LeftB,LeftP),
       RightP=Right ; [F]=Right, Pol=0, LeftP=Left, RightP=[]),
      sep_fml(F,Pol,inv,L1,R1,L2,R2,S,FreeV,V,Cpy,Cpy1), ! ;
      (append(LeftA,[F|LeftB],Left), Pol=1, append(LeftA,LeftB,LeftP),
       RightP=Right ; [F]=Right, Pol=0, LeftP=Left, RightP=[]),
      sep_fml(F,Pol,nin,L1,R1,L2,R2,S,FreeV,V,Cpy,Cpy1)
    ),
    ( V=[] -> true ; \+ length(FreeV,VarLim) ),
    copy_term((Cpy,FreeV),(Cpy1,FreeV)), append(FreeV,V,FreeV1),
    append(LeftP,L1,Left1), ( R1=[] -> Right1=RightP ; Right1=R1 ),
    append(LeftP,L2,Left2), ( R2=[] -> Right2=RightP ; Right2=R2 ),
    sep_prove(Left1,Right1,l(S),FreeV1,VarLim),
    ( L2=[],R2=[] -> true ; sep_prove(Left2,Right2,r(S),FreeV1,VarLim) ).

sep_prove(Left,Right,_,_,_) :-
    member(F,Left), unify_with_occurs_check([F],Right).
