%% Version: 1.0  -  Date: 25/11/2004  -  File: ileancop_swi.pl
%%
%% Purpose: ileanCoP: A Connection Prover for Intuitionistic Logic
%%
%% Author:  Jens Otten
%% Web:     www.leancop.de/ileancop/
%%
%% Usage:   prove(F).   % where F is a first-order formula
%%                      %  e.g. F=((p,all X:(p=>q(X)))=>all Y:q(Y))
%%
%% Copyright: (c) 2005 by Jens Otten
%% License:   GNU General Public License


%:- [nnf_mm_intu].

%% Version: 1.1  -  Date: 04/11/2004  -  File: nnf_mm_intu.pl
%%
%% Purpose: - computes Skolemized negation normal form for a
%%            first-order formula (based on leanTAP's nnf.pl)
%%          - computes disjunctive normal form for a formula
%%            in Skolemized negational normal form
%%          - computes a matrix form (and applies MULT and TAUT
%%            reductions) for a formula in disjunctive normal form
%%          - renames variables so that a unique variable name
%%            is assigned to each quantifier in a formula
%%          - add prefix to each literal (variable/constant for
%%            negative/positive occurence of '~', '=>', 'all X:')
%%          - add set of tuples [X,P] to each clause for each
%%            free variable X with prefix P occuring in clause
%%
%% Author:  Jens Otten
%% Web:     www.leancop.de/ileancop/
%%
%% Usage:   make_matrix_intu(F,M). % where F is a first-order formula
%%                                 % and its clausal form M is returned,
%%                                 % where prefixes are added to literals
%%                                 % and sets of free variables to clauses
%%
%% Example: make_matrix(ex Y: (all X: ((p(Y) => p(X))) ), Matrix).
%%          Matrix = [[[X1, []]] : [-(p(X1)) : -([1^[X1], 2^[X1]])],
%%                    [] : [p(1^[X1]^[1^[X1]]) : [1^[X1], 2^[X1]]]]
%%
%% Copyright: (c) 2005 by Jens Otten
%% License:   GNU General Public License


coprove(F) :- map_operators(F,G),coprove1(G).

coprove1(F) :-  
  make_matrix_intu(F,M), 
  coprove(M,1),
  !.
  
  
% connectives and quantifiers

:- op(1130, xfy, <=>). % equivalence
:- op(1110, xfy, =>).  % implication
%                      % disjunction (;)
%                      % conjunction (,)
%:- op( 500, fy, ~).    % negation
:- op( 500, fy, all).  % universal quantifier
:- op( 500, fy, ex).   % existential quantifier
%:- op( 500,xfy, :).

% -----------------------------------------------------------------
%  nnf(+Fml,?NNF)
%
% Fml is a first-order formula and NNF its Skolemized negation
% normal form (where quantifiers have been removed from NNF).
%
% Syntax of Fml:
%  negation: '~', disj: ';', conj: ',', impl: '=>', eqv: '<=>',
%  quant. 'all X:<Formula>', 'ex X:<Formula>' where 'X' is a
%  prolog variable.
%
% Syntax of NNF: negation: '-', disj: ';', conj: ','.
%
% Example: nnf(ex Y: (all X: ((p(Y) => p(X))) ),NNF).
%          NNF = (-(p(X1^[]))) : [1^[X1], 2^[X1]] ;
%                p(1^[X1]^[1^[X1]]) : [1^[X1], 2^[X1]]

nnf(Fml,NNF) :- nnf(Fml:[],[],NNF,_,1,_).

% -----------------------------------------------------------------
%  nnf(+Fml,+FreeV,-NNF,-Paths,+I,-I1)
%
% Fml,NNF: See above.
% FreeV:   List of free variables in Fml.
% Paths:   Number of disjunctive paths in Fml.
% I,I1:    Integer marks unique position of Fml.

nnf(Fml:Pre,FreeV,NNF,Paths,I,I1) :-
    (Fml = ~A         -> Fml1 = -(A),                     Pre1=[I^FreeV];
     Fml = -(~A)      -> Fml1 = A,                        Pre1=[_];
     Fml = -(all X:F) -> Fml1 = (ex X: -F),               Pre1=[_];
     Fml = -(ex X:F)  -> Fml1 = (all X: -F),              Pre1=[];
     Fml = -((A ; B)) -> Fml1 = ((-A , -B)),              Pre1=[];
     Fml = -((A , B)) -> Fml1 = (-A ; -B),                Pre1=[];
     Fml = (A => B)   -> Fml1 = (-A ; B),                 Pre1=[I^FreeV];
     Fml = -((A => B))-> Fml1 = ((A , -B)),               Pre1=[_];
     Fml = (A <=> B)  -> Fml1 = ((A => B) , (B => A)),    Pre1=[];
     Fml = -((A<=>B)) -> Fml1 = -(((A => B) , (B => A))), Pre1=[] ), !,
    append(Pre,Pre1,Pre2), I2 is I+1,
    ([Pre3]=Pre1,var(Pre3) -> FreeV1=[Pre3|FreeV] ;
                              FreeV1=FreeV),
    nnf(Fml1:Pre2,FreeV1,NNF,Paths,I2,I1).

nnf((ex X:F):Pre,FreeV,NNF,Paths,I,I1) :- !,
    copy_term((X,F,FreeV),((X1^Pre),F1,FreeV)),
    nnf(F1:Pre,[X1|FreeV],NNF,Paths,I,I1).

nnf((all X:Fml):Pre,FreeV,NNF,Paths,I,I1) :- !,
    copy_term((X,Fml,FreeV),((I^FreeV^Pre1),Fml1,FreeV)),
    (Fml= -_ -> Pre1=Pre ;
                append(Pre,[I^FreeV],Pre1)),
    I2 is I+1,
    nnf(Fml1:Pre1,FreeV,NNF,Paths,I2,I1).

nnf((A ; B):Pre,FreeV,NNF,Paths,I,I1) :- !,
    nnf(A:Pre,FreeV,NNF1,Paths1,I,I2),
    nnf(B:Pre,FreeV,NNF2,Paths2,I2,I1),
    Paths is Paths1 * Paths2,
    (Paths1 > Paths2 -> NNF = (NNF2;NNF1);
                        NNF = (NNF1;NNF2)).

nnf((A , B):Pre,FreeV,NNF,Paths,I,I1) :- !,
    nnf(A:Pre,FreeV,NNF1,Paths1,I,I2),
    nnf(B:Pre,FreeV,NNF2,Paths2,I2,I1),
    Paths is Paths1 + Paths2,
    (Paths1 > Paths2 -> NNF = (NNF2,NNF1);
                        NNF = (NNF1,NNF2)).

nnf(Lit,_,Lit,1,I,I).

% -----------------------------------------------------------------
%  make_matrix(+Fml,-Matrix)
%
% Syntax of Fml:
%  negation: '~', disj: ';', conj: ',', impl: '=>', eqv: '<=>',
%  quant. 'all X:<Formula>', 'ex X:<Formula>' where 'X' is a
%  prolog variable.
%
% Syntax of Matrix:
%  Matrix is a list of clauses; a clause is a list of literals.
%  Each clause has the form FreeV:Cla where Cla is a list of literals
%  and FreeV is a list which contains one tuple [X,P] for each (free)
%  variable X with prefix P occuring in Cla. Each literal has the
%  form Lit:Pre where Lit is a literal (negation is expressed by '-')
%  and Pre is a the prefix of Lit.
%
% Example: make_matrix(ex Y: (all X: ((p(Y) => p(X))) ),Matrix).
%          Matrix = [[[X1, []]] : [(-(p(X1))) : -([1^[X1], 2^[X1]])],
%                   [] : [p(1^[X1]^[1^[X1]]) : [1^[X1], 2^[X1]]]]

make_matrix_intu(Fml,Matrix) :-
    univar(Fml,[],Fml1), nnf(Fml1,NNF), dnf(NNF,DNF),
    mat(DNF,Matrix1), matvar(Matrix1,Matrix).

% -----------------------------------------------------------------
%  dnf(+NNF,-DNF)
%
% Syntax of NNF, DNF: negation: '-', disj: ';', conj: ','.
%
% Example: dnf(((p:[] ; (-(p)):[1^[]]), (q:[] ; (-(q)):[2^[]])),DNF).
%          DNF = (p:[], q:[] ; p:[], (-(q)):[2^[]]) ;
%                (-(p)):[1^[]], q:[] ; (-(p)):[1^[]], (-(q)):[2^[]]

dnf(((A;B),C),(F1;F2)) :- !, dnf((A,C),F1), dnf((B,C),F2).
dnf((A,(B;C)),(F1;F2)) :- !, dnf((A,B),F1), dnf((A,C),F2).
dnf((A,B),F) :- !, dnf(A,A1), dnf(B,B1),
    ( (A1=(_C;_D);B1=(_C;_D)) -> dnf((A1,B1),F) ; F=(A1,B1) ).
dnf((A;B),(A1;B1)) :- !, dnf(A,A1), dnf(B,B1).
dnf(Lit,Lit).

% -----------------------------------------------------------------
%  mat(+DNF,-Matrix)
%
% Syntax of DNF,Matrix: See above.
%
% Example:  mat(((p:[], q:[] ; p:[], (-(q)):[2^[]]) ; (-(p)):[1^[]],
%                q:[] ; (-(p)):[1^[]], (-(q)):[2^[]]),Matrix).
%           Matrix = [[p:[], q:[]], [p:[], (-(q)): -([2^[]])],
%                     [(-(p)): -([1^[]]), q:[]],
%                     [(-(p)): -([1^[]]), (-(q)): -([2^[]])]]

mat((A;B),M) :- !, mat(A,MA), mat(B,MB), append(MA,MB,M).
mat((A,B),M) :- !,
    ( mat(A,[CA]), mat(B,[CB]) -> union2(CA,CB,M) ; M=[] ).
mat(-(Lit):Pre,[[-(Lit):(-Pre)]]) :- !.
mat(Lit:Pre,[[Lit:Pre]]).

% -----------------------------------------------------------------
%  univar(+Fml,+Var,-Fml1)
%
% Fml is a first-order formula and Fml1 is equivalent to Fml where
% all variables in Fml have been replaced with unique variable names
% (Var is the list of variables which have been collected so far)
%
% Syntax of Fml, Fml1: See above.
%
% Example: univar((all X:(p(X),ex X:q(X))),[],Fml1).
%          Fml1 = all X1 : (p(X1), ex X2 : q(X2))

univar(Fml,_,Fml)    :- (atomic(Fml);var(Fml);Fml==[[]]), !.

univar(Fml,Var,Fml1) :-
    Fml=..[Op,Arg|ArgL],
    ( (Op=ex;Op=all) -> Arg=X:F, delete2(Var,X,Var1),
                        copy_term((X,F,Var1),(Y,F1,Var1)),
                        univar(F1,[Y|Var],F2), Fml1=..[Op,Y:F2]
                        ;
                        univar(Arg,Var,Arg1), univar(ArgL,Var,ArgL1),
                        Fml1=..[Op,Arg1|ArgL1] ).

% -----------------------------------------------------------------
%  union2/member2  (realizes MULT/TAUT reductions)

union2([],L,[L]).
union2([X|L1],L2,M)    :- member2(X,L2), !, union2(L1,L2,M).
union2([X:Pre|_],L2,M) :- (-(Xn):(-Pr)=X:Pre;-(X):(-Pre)=Xn:Pr) ->
                          member2(Xn:Pr,L2), !, M=[].
union2([X|L1],L2,M)    :- union2(L1,[X|L2],M).

member2(X,[Y|_]) :- X==Y, !.
member2(X,[_|T]) :- member2(X,T).

% -----------------------------------------------------------------
%  delete2 (deletes variable from list)

delete2([],_,[]).
delete2([X|T],Y,T1)     :- X==Y, !, delete2(T,Y,T1).
delete2([X|T],Y,[X|T1]) :- delete2(T,Y,T1).

% -----------------------------------------------------------------
%  matvar(+Mat,-Mat1)
%
% Mat is a matrix and Mat1 is a matrix where each clause Cla of Mat has
% been replaced by FreeV:Cla where FreeV is a list which contains one
% tuple [X,P] for each (free) variable X with prefix P occuring in Cla
%
% Syntax of Mat,Mat1: See above.
%
% Example: matvar([[p(1^[]^[1^[]]):[1^[]], q(X1^[1^[]]):[1^[]]]],Mat1).
%          Mat1 = [[[X1,[1^[]]]]:[p(1^[]^[1^[]]):[1^[]], q(X1):[1^[]]]]

matvar([],[]).
matvar([Cla|Mat],[FreeV:Cla1|Mat1]) :-
    clavar(Cla,Cla1,FreeV), matvar(Mat,Mat1).

clavar(Fml^Pre,Fml,[[Fml,Pre]]) :- var(Fml), !.
clavar(Fml,Fml,[])              :- (atomic(Fml);Fml==[[]];Fml=_^_), !.
clavar(Fml:Pre,Fml1:Pre,FreeV)  :- !, clavar(Fml,Fml1,FreeV).
clavar(Fml,Fml1,FreeV) :-
    Fml=..[Op,Arg|ArgL],
    clavar(Arg,Arg1,FreeV1), clavar(ArgL,ArgL1,FreeV2),
    union2(FreeV1,FreeV2,[FreeV]), Fml1=..[Op,Arg1|ArgL1].
    

:- dynamic(pathlim/0).

%%% prove formula F




orig_prove(F) :- Time1 is cputime, make_matrix_intu(F,M), coprove(M,1),
            Time2 is cputime, Time is Time2-Time1, print(Time).

coprove(Mat,PathLim) :-
     append(MatA,[FV:Cla|MatB],Mat), \+member(-(_):_,Cla),
     append(MatA,MatB,Mat1),
     prove([!:[]],[FV:[-(!):(-[])|Cla]|Mat1],[],PathLim,[PreSet,FreeV]),
     check_addco(FreeV), prefix_unify(PreSet).

coprove(Mat,PathLim) :-
     retract(pathlim), PathLim1 is PathLim+1, coprove(Mat,PathLim1).

prove([],_,_,_,[[],[]]).
prove([Lit:Pre|Cla],Mat,Path,PathLim,[PreSet,FreeV]) :-
     (-NegLit=Lit;-Lit=NegLit) ->
        ( member(NegL:PreN,Path), unify_with_occurs_check(NegL,NegLit),
          \+ \+ prefix_unify([Pre=PreN]), PreSet1=[], FreeV3=[]
          ;
          append(MatA,[Cla1|MatB],Mat), copy_term(Cla1,FV:Cla2),
          append(ClaA,[NegL:PreN|ClaB],Cla2),
          unify_with_occurs_check(NegL,NegLit),
          \+ \+ prefix_unify([Pre=PreN]),
          append(ClaA,ClaB,Cla3),
          ( Cla1==FV:Cla2 ->
               append(MatB,MatA,Mat1)
               ;
               length(Path,K), ( K<PathLim ->
               append(MatB,[Cla1|MatA],Mat1) ;
               \+ pathlim -> assert(pathlim), fail )
          ),
          prove(Cla3,Mat1,[Lit:Pre|Path],PathLim,[PreSet1,FreeV1]),
          append(FreeV1,FV,FreeV3)
        ),
        prove(Cla,Mat,Path,PathLim,[PreSet2,FreeV2]),
        append([Pre=PreN|PreSet1],PreSet2,PreSet),
        append(FreeV2,FreeV3,FreeV).


%%% prefix unification

prefix_unify([]).
prefix_unify([S=T|G]) :- (-S2=S -> T2=T ; -S2=T, T2=S),
                         flatten([S2,_],S1), flatten(T2,T1),
                         tunify1(S1,[],T1), prefix_unify(G).

tunify1([],[],[]).
tunify1([],[],[X|T])       :- tunify1([X|T],[],[]).
tunify1([X1|S],[],[X2|T])  :- (var(X1) -> (var(X2), X1==X2);
                             (\+var(X2), unify_with_occurs_check(X1,X2))),
                             !, tunify1(S,[],T).
tunify1([C|S],[],[V|T])    :- \+var(C), !, var(V), tunify1([V|T],[],[C|S]).
tunify1([V|S],Z,[])        :- unify_with_occurs_check(V,Z), tunify1(S,[],[]).
tunify1([V|S],[],[C1|T])   :- \+var(C1), V=[], tunify1(S,[],[C1|T]).
tunify1([V|S],Z,[C1,C2|T]) :- \+var(C1), \+var(C2), append(Z,[C1],V1),
                             unify_with_occurs_check(V,V1),
                             tunify1(S,[],[C2|T]).
tunify1([V,X|S],[],[V1|T])     :- var(V1), tunify1([V1|T],[V],[X|S]).
tunify1([V,X|S],[Z1|Z],[V1|T]) :- var(V1), append([Z1|Z],[Vnew],V2),
                                 unify_with_occurs_check(V,V2),
                                 tunify1([V1|T],[Vnew],[X|S]).
tunify1([V|S],Z,[X|T])     :- (S=[]; T\=[]; \+var(X)) ->
                             append(Z,[X],Z1), tunify1([V|S],Z1,T).

%%% check additional quantifier/prefix interaction condition

check_addco([]).
check_addco([[X,Pre]|L]) :- addco(X,Pre), check_addco(L).

addco(X,_)          :- (atomic(X);var(X);X==[[]]), !.
addco(_^_^Pre1,Pre) :- !, prefix_unify([-Pre1=Pre]).
addco(T,Pre)        :- T=..[_,T1|T2], addco(T1,Pre), addco(T2,Pre).
