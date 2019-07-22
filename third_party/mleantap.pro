%% File: mleantap13_swi.pl  -  Version: 1.3  -  Date: 24th February 2012
%%
%% Purpose: MleanTAP: A Tableau Theorem Prover for Modal Logic
%%
%% Author:  Jens Otten
%% Web:     www.leancop.de/mleantap/
%%
%% Usage:   prove(F).   % where F is a modal first-order formula
%%                      %  e.g. F=(all X: ex Y:(~ *q, #p(Y)=>p(X)))
%%
%% Copyright: (c) 2006-2012 by Jens Otten
%% License:   GNU General Public License

logic(s4).       % specify modal logic (d,t,s4,s5)
domain(cumul).   % specify domain condition (const,cumul,vary)


:- op(1130, xfy, <=>). :- op(1110, xfy, =>). :- op(500, fy, ~).
:- op( 500,  fy, all). :- op( 500,  fy, ex). :- op(500,xfy, :).
:- op( 500,  fy, #).   :- op( 500,  fy, *).

%%% prove formula F

%prove(F) :- A is cputime, pp(F,1), B is cputime, C is B-A, print(C).

modprove(F) :- map_operators(F,T),pp(T,1),!.

pp(F,A) :-  % display(A), nl, 
           prove([(F,0),[],[],l],[],[],[],[],A,[W,FV]),
           domain_cond(FV), mod_prefix_unify(W).
pp(F,A) :- \+ no_mul([(F,0)]), A1 is (A+1), pp(F,A1).

%%% check existence of multiplicities

no_mul([]).
no_mul([(F,Pol)|T]):- fml(F,Pol,_,_,_,F1,F2,F3,_,_,_,FrV,PrV,_,_,U,U), !,
                      FrV==[], PrV==[], no_mul([F1,F2,F3|T]).
no_mul([_|T]):- no_mul(T).

%%% specification of tableau rules for all connectives and quantifiers

fml((A,B),  1, _,_,_,(A,1),[],(B,1),[],      _,_,  [],[],  [],[],  [],[]).
fml((A,B),  0, _,_,_,(A,0),(B,0),[],[],      _,_,  [],[],  [],[],  [],[]).
fml((A;B),  1, _,_,_,(A,1),(B,1),[],[],      _,_,  [],[],  [],[],  [],[]).
fml((A;B),  0, _,_,_,(A,0),[],(B,0),[],      _,_,  [],[],  [],[],  [],[]).
fml((A<=>B),Pl,_,_,_,(((A=>B),(B=>A)),Pl),[],[],[],_,_,[],[],[],[],[],[]).
fml((A=>B),1,_,_,_, (A,0),(B,1),[],[],       _,_,  [],[],  [],[],  [],[]).
fml((A=>B),0,_,_,_, (B,0),[],(A,1),[],       _,_,  [],[],  [],[],  [],[]).
fml((~A),  1,_,_,_, (A,0),[],   [],[],       _,_,  [],[],  [],[],  [],[]).
fml((~A),  0,_,_,_, (A,1),[],   [],[],       _,_,  [],[],  [],[],  [],[]).
fml(all X:A,1,_,_,_,(C,1),[],[],(all X:A,1), FrV,_,FrV,[],  Y,[],X:A,Y:C).
fml(all X:A,0,Pr,FV,S,(C,0),[],[],[],  _,_,[],[],[],[],(X,A),(S^FV^Pr,C)).
fml(ex X:A, 1,Pr,FV,S,(C,1),[],[],[],  _,_,[],[],[],[],(X,A),(S^FV^Pr,C)).
fml(ex X:A, 0,_,_,_,(C,0),[],[],(ex X:A,0),  FrV,_,FrV,[],  Y,[],X:A,Y:C).
fml((#A),   1,_,_,_, (C,1),[],[],((#A),1),   _,PrV,[],PrV, [],[_],  A,C ).
fml((#A),   0,_,FV,S,(A,0),[],[],[],         _,_,  [],[],[],[S^FV],[],[]).
fml((*A),   1,_,FV,S,(A,1),[],[],[],         _,_,  [],[],[],[S^FV],[],[]).
fml((*A),   0,_,_,_, (C,0),[],[],((*A),0),   _,PrV,[],PrV, [],[_],  A,C ).

%%% tableau core prover

prove([(F,Pol),Pre,FreeV,S],UnExp,Lits,FrV,PrV,VarLim,[PU,AC]) :-
  fml(F,Pol,Pre1,FreeV,S,F1,F2,FUnE,FUnE1,FrV,PrV,Lim1,Lim2,V,PrN,Cpy,Cpy1),
  !, \+length(Lim1,VarLim), \+length(Lim2,VarLim),
  copy_term((Cpy,FreeV),(Cpy1,FreeV)),  append(Pre,PrN,Pre1),
  (FUnE =[] -> UnExp2=UnExp ; UnExp2=[[FUnE,Pre1,FreeV,r(S)]|UnExp]),
  (FUnE1=[] -> UnExp1=UnExp2 ; append(UnExp2,[[FUnE1,Pre,FreeV,S]],UnExp1)),
  (var(V)   -> FV2=[V|FreeV], FrV1=[V|FrV], AC2=[[V,Pre1]|AC1] ;
               FV2=FreeV,     FrV1=FrV,     AC2=AC1),
  (PrN\=[!] -> FreeV1=FV2, PrV1=PrV ; FreeV1=[PrN|FV2], PrV1=[PrN|PrV]),
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


%%% prefix unification for D, T, S4, S5

mod_prefix_unify([]).
mod_prefix_unify([S=T|G]) :-
    ( logic(s5) -> S1=S, T1=T ; flatten(S,S1,[]), flatten(T,T1,[]) ),
    ( logic(s5) -> tuni_s5(S1,T1) ;
      logic(d)  -> tuni_d(S1,T1) ;
      logic(t)  -> tuni_t(S1,[],T1) ;
      logic(s4) -> tunify(S1,[],T1) ),
    mod_prefix_unify(G).

flatten(A,[A|B],B) :- (var(A) ; A=_^_), !.
flatten([],A,A).
flatten([A|B],C,D) :- flatten(A,C,E), flatten(B,E,D).

%%% rules for D

tuni_d([],[]).
tuni_d([X1|S],[X2|T]) :- unify_with_occurs_check(X1,X2), tuni_d(S,T).

%%% rules for T

tuni_t([],[],[]).
tuni_t([],[],[X|T])      :- tuni_t([X|T],[],[]).
tuni_t([V|S],[],[])      :- V=[], tuni_t(S,[],[]).
tuni_t([X1|S],[],[X2|T]) :- (var(X1) -> (var(X2), X1==X2);
                            (\+var(X2), unify_with_occurs_check(X1,X2))),
                            !, tuni_t(S,[],T).
tuni_t([V|S],[],[X|T])   :- var(V), tuni_t([V|S],[X],T).
tuni_t([C|S],[],[V|T])   :- \+var(C), var(V), tuni_t([V|T],[C],S).
tuni_t([V1|S],[],[V2|T]) :- var(V1), V2=[], tuni_t(T,[V1],S).
tuni_t([V|S],[X],T)      :- V=[], tuni_t(S,[X],T).
tuni_t([V|S],[X],T)      :- var(V), unify_with_occurs_check(V,X),
                            tuni_t(S,[],T).
tuni_t([C|S],[V],T)      :- \+var(C), var(V), unify_with_occurs_check(V,C),
                            tuni_t(S,[],T).

%%% rules for S4

tunify([],[],[]).
tunify([],[],[X|T])       :- tunify([X|T],[],[]).
tunify([X1|S],[],[X2|T])  :- (var(X1) -> (var(X2), X1==X2);
                             (\+var(X2), unify_with_occurs_check(X1,X2))),
                             !, tunify(S,[],T).
tunify([C|S],[],[V|T])    :- \+var(C), !, var(V), tunify([V|T],[],[C|S]).
tunify([V|S],Z,[])        :- unify_with_occurs_check(V,Z), tunify(S,[],[]).
tunify([V|S],[],[C1|T])   :- \+var(C1), V=[], tunify(S,[],[C1|T]).
tunify([V|S],Z,[C1,C2|T]) :- \+var(C1), \+var(C2), append(Z,[C1],V1),
                             unify_with_occurs_check(V,V1),
                             tunify(S,[],[C2|T]).
tunify([V,X|S],[],[V1|T]) :- var(V1), tunify([V1|T],[V],[X|S]).
tunify([V,X|S],[Z1|Z],[V1|T]) :- var(V1), append([Z1|Z],[Vnew],V2),
                                 unify_with_occurs_check(V,V2),
                                 tunify([V1|T],[Vnew],[X|S]).
tunify([V|S],Z,[X|T])     :- (S=[]; T\=[]; \+var(X)) ->
                             append(Z,[X],Z1), tunify([V|S],Z1,T).

%%% rules for S5

tuni_s5([],[]).
tuni_s5(S,[]) :- append(_,[X1],S), X1=[].
tuni_s5([],T) :- append(_,[X1],T), X1=[].
tuni_s5(S,T)  :- append(_,[X1],S), append(_,[X2],T),
                 unify_with_occurs_check(X1,X2).

%%% check domain condition

domain_cond(FV) :- domain(const) -> true ; domcond(FV,FV).

domcond([],_).
domcond([[X,Pre]|L],FV) :- domco(X,Pre,FV), domcond(L,FV).

domco(X,_,_)    :- (atomic(X);X==[[]]), !.
domco(X,Pre,FV) :- var(X), !, ( \+ domain(vary) -> true ;
                   domcom(X,FV,Pre1), dom_unify(Pre1,Pre) ).
domco(_^_^Pre1,Pre,_) :- !, dom_unify(Pre1,Pre).
domco(T,Pre,FV) :- T=..[_,U|V], domco(U,Pre,FV), domco(V,Pre,FV).

domcom(X,[[X1,Pre1]|FV],Pre) :- X==X1 -> Pre=Pre1 ; domcom(X,FV,Pre).

dom_unify(Pre1,Pre2) :-
    ( logic(s5) -> S=Pre1, T=Pre2 ;
                   flatten(Pre1,S,[]), flatten(Pre2,T,[]) ),
    ( domain(vary) ->
      ( logic(s5) -> tuni_s5(S,T) ;
        logic(d)  -> tuni_d(S,T) ;
        logic(t)  -> tuni_t(S,[],T) ;
        logic(s4) -> tunify(S,[],T) )
      ;
      domain(cumul) ->
      ( logic(s5) -> true ;
        logic(d)  -> length(S,LenS), append(T1,_,T), length(T1,LenS),
                     tuni_d(S,T1) ;
        logic(t)  -> append(T1,_,T), tuni_t(S,[],T1),
                     ( append(_,[X],T1) ->  X\==[] ; true ) ;
        logic(s4) -> append(S,[_],S1), tunify(S1,[],T) )
    ).

