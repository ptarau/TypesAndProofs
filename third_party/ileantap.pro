%% Version:  1.17m  -  Date:  7/05/97  -  File: ileantap_swi.pl
%%
%% Purpose: ileanTAP: An Intuitionistic Theorem Prover
%%
%% Author:  Jens Otten
%% Web:     www.leancop.de/ileantap/
%%
%% Usage:   ilprove(F).   % where F is a first-order formula
%%                      %  e.g. F=(all X: ex Y:(~q,p(Y)->p(X)))
%%
%% Copyright: (c) 1997 by Jens Otten
%% License:   GNU General Public License

% ADAPTED TO SUPPORT THE SAME OPERATORS as other provers
% very slow, times out at ptest(9,ilprove), ntest(4,ilprove)
% possibly buggy - uses some form of loop check that might fail
% on non-theorems
% eg. ?- ilprove((a->a)->a).

%:- op(1130, xfy, <->). :- op(1110, xfy, ->). :- op(500, fy, ~).
:- op( 500,  fy, all). :- op( 500,  fy, ex). :- op(500,xfy, :).


%%% Prove first-order formula F


ilprove(F):-  
 ilprove(F,1).

ilprove(F,A) :- prove([(F,0),[],[],l],[],[],[],[],A,[W,Z]),
           t_string_unify(W,Z).
ilprove(F,A) :- \+no_mul([(F,0)]), A1 is (A+1), ilprove(F,A1).


%%% Check Multiplicities (added 17/09/04)

no_mul([]).
no_mul([(F,Pol)|T]):- fml(F,Pol,_,_,_,F1,F2,F3,_,_,_,FrV,PrV,_,_,U,U), !,
                      FrV==[], PrV==[], no_mul([F1,F2,F3|T]).
no_mul([_|T]):- no_mul(T).


%%% Connective/Quantifier Definitions

fml((A,B),  1, _,_,_,(A,1),[],(B,1),[],        _,_,  [],[], [],[],  [],[]).
fml((A,B),  0, _,_,_,(A,0),(B,0),[],[],        _,_,  [],[], [],[],  [],[]).
fml((A v B),  1, _,_,_,(A,1),(B,1),[],[],        _,_,  [],[], [],[],  [],[]).
fml((A v B),  0, _,_,_,(A,0),[],(B,0),[],        _,_,  [],[], [],[],  [],[]).
fml((A<->B),Pl,_,_,_,(((A->B),(B->A)),Pl),[],[],[],_,_,[],[],[],[],[],[]).
fml((A->B),1,_,_,_, (C,0),(D,1),[],((A->B),1),_,PrV,[],PrV,[],_,A:B,C:D).
fml((A->B),0,_,FV,S,(B,0),[],(A,1),[],        _,_,  [],[], [],S^FV,[],[]).
fml((~A),  1,_,_,_, (C,0),[],[],((~A),1),     _,PrV,[],PrV,[],_,    A,C).
fml((~A),  0,_,FV,S,(A,1),[],[],[],           _,_,  [],[], [],S^FV,[],[]).
fml(all X:A,1,_,_,_,(C,1),[],[],(all X:A,1),FrV,PrV,FrV,PrV,Y,_,X:A,Y:C).
fml(all X:A,0,Pr,FV,S,(C,0),[],[],[],_,_,[],[],[],S^FV,(X,A),(S^[]^Pr,C)).
fml(ex X:A, 1,Pr,FV,S,(C,1),[],[],[],_,_,[],[],[],[],  (X,A),(S^FV^Pr,C)).
fml(ex X:A, 0,_,_,_,(C,0),[],[],(ex X:A,0), FrV,_,  FrV,[],Y,[],X:A,Y:C).


%%% Path Checking

prove([(F,Pol),Pre,FreeV,S],UnExp,Lits,FrV,PrV,VarLim,[PU,AC]) :-
  fml(F,Pol,Pre1,FreeV,S,F1,F2,FUnE,FUnE1,FrV,PrV,Lim1,Lim2,V,PrN,Cpy,Cpy1),
  !, \+length(Lim1,VarLim), \+length(Lim2,VarLim),
  copy_term((Cpy,FreeV),(Cpy1,FreeV)),  append(Pre,[PrN],Pre1),
  (FUnE =[] -> UnExp2=UnExp ; UnExp2=[[FUnE,Pre1,FreeV,r(S)]|UnExp]),
  (FUnE1=[] -> UnExp1=UnExp2 ; append(UnExp2,[[FUnE1,Pre,FreeV,S]],UnExp1)),
  (var(V)   -> FV2=[V|FreeV], FrV1=[V|FrV], AC2=[[V,Pre1]|AC1] ;
               FV2=FreeV,     FrV1=FrV,     AC2=AC1),
  (var(PrN) -> FreeV1=[PrN|FV2], PrV1=[PrN|PrV] ; FreeV1=FV2, PrV1=PrV),
  prove([F1,Pre1,FreeV1,l(S)],UnExp1,Lits,FrV1,PrV1,VarLim,[PU1,AC1]),
  (F2=[] -> PU=PU1, AC=AC2 ;
  prove([F2,Pre1,FreeV1,r(S)],UnExp1,Lits,FrV1,PrV1,VarLim,[PU2,AC3]),
  append(PU1,PU2,PU), append(AC2,AC3,AC)).

prove([(Lit,Pol),Pre|_],_,[[(L,P),Pr|_]|Lits],_,_,_,[PU,AC]) :-
   ( unify_with_occurs_check(Lit,L),
     Pol is 1-P, (Pol=1 -> PU=[Pre=Pr] ; PU=[Pr=Pre]), AC=[]) ;
   prove([(Lit,Pol),Pre|_],[],Lits,_,_,_,[PU,AC]).
prove(Lit,[Next|UnExp],Lits,FrV,PrV,VarLim,PU_AC) :-
   prove(Next,UnExp,[Lit|Lits],FrV,PrV,VarLim,PU_AC).


%%% T-String Unification

t_string_unify([],AC)     :- addco(AC,[],final).
t_string_unify([S=T|G],AC):- flatten([S,_],S1,[]), flatten(T,T1,[]),
                             tunify(S1,[],T1), addco(AC,[],t),
                             t_string_unify(G,AC).
tunify([],[],[]).
tunify([],[],[X|T])           :- tunify([X|T],[],[]).
tunify([X1|S],[],[X2|T])      :- (var(X1) -> (var(X2), X1==X2);
                                 (\+var(X2), unify_with_occurs_check(X1,X2))),
                                 !, tunify(S,[],T).
tunify([C|S],[],[V|T])        :- \+var(C), !, var(V), tunify([V|T],[],[C|S]).
tunify([V|S],Z,[])            :- unify_with_occurs_check(V,Z),
                                 tunify(S,[],[]).
tunify([V|S],[],[C1|T])       :- \+var(C1), V=[], tunify(S,[],[C1|T]).
tunify([V|S],Z,[C1,C2|T])     :- \+var(C1), \+var(C2), append(Z,[C1],Vu),
                                 unify_with_occurs_check(V,Vu),
                                 tunify(S,[],[C2|T]).
tunify([V,X|S],[],[V1|T])     :- var(V1), tunify([V1|T],[V],[X|S]).
tunify([V,X|S],[Z1|Z],[V1|T]) :- var(V1), append([Z1|Z],[Vnew],Vu),
                                 unify_with_occurs_check(V,Vu),
                                 tunify([V1|T],[Vnew],[X|S]).
tunify([V|S],Z,[X|T])         :- (S=[]; T\=[]; \+var(X)) ->
                                 append(Z,[X],Z1), tunify([V|S],Z1,T).

addco(X,_,_)          :- (var(X); X=[[]]), !.
addco([[X,Pre]|L],[],Ki):- !, addco(X,Pre,Ki), addco(L,[],Ki).
addco(_^_^Pre1,Pre,Ki):- !, (Ki=final-> flatten(Pre1,S,[]),flatten(Pre,T,[]),
                         append(S,_,T); \+ \+ t_string_unify([Pre1=Pre],[])).
addco(T,Pre,Ki)       :- T=..[_,T1|T2],!, addco(T1,Pre,Ki), addco(T2,Pre,Ki).
addco(_,_,_).

flatten(A,[A|B],B) :- (var(A); A=_^_), !.
flatten([],A,A).
flatten([A|B],C,D) :- flatten(A,C,E), flatten(B,E,D).
