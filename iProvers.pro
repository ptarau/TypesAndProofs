% minimal logic prover, via generate and test

intu(T):-intu(16,T,_).

intu(T0,X):-intu(16,T0,X).

intu(N,T0,X):-intu(N,T0,X,_),!.

% generate normal forms of lambda tems of size N, 
% then try to see if any subsumes the query type T0
% if so, we have found an inhabitant X
% in fact, if generatting larger and 
intu(N,GT,X,T):-
  %varvars(GT,T0), % works if ground or not
  GT=T0,
  tnfs(N,X,T),subsumes_term(T,T0).

intu0(T0,X):-N=16,intu0(N,T0,X).

intu0(N,T0,X):-tnfs(N,X,T),T0=@=T.


% Dyckhoff's original LJT   
dprove(A):-provable(A),!.
  

% derived directly from Dyckhoff's LJT calculus
lprove(T):-ljt(T,[]),!. % done

ljt(A,Vs):-memberchk(A,Vs),!. % L1

ljt((A->B),Vs):-!,ljt(B,[A|Vs]). % L2

ljt(G,Vs1):- %atomic(G),                % L3
  select((A->B),Vs1,Vs2),
  memberchk(A,Vs2),
  !,
  ljt(G,[B|Vs2]).

ljt(G,Vs1):- % atomic(G),              % L4
  select( ((C->D)->B),Vs1,Vs2),
  % ljt((C->D), [C(D->B)|Vs2]),  
  ljt(D, [C,(D->B)|Vs2]),   
  !,
  ljt(G,[B|Vs2]).

% variant
llprove(T):-lljt(T,[]),!.

lljt(A,Vs):-memberchk(A,Vs),!.

lljt((A->B),Vs):-!,lljt(B,[A|Vs]).

lljt(G,Vs1):-
  select((A->B),Vs1,Vs2),
  memberchk(A,Vs2),
  !,
  lljt((B->G),Vs2).

lljt(G,Vs1):-
  select( ((C->D)->B), Vs1,Vs2),
  lljt(((D->B)->(C->D)->B),Vs2),
  !,
  lljt((G),Vs2).



% simplest, with multisets, no contraction
% with a single select/3 operation

bprove(T):-ljb(T,[]).

%ljb(A,Vs):-ppp((Vs-->A)),fail. % fo traing only

ljb(A,Vs):-memberchk(A,Vs),!.
ljb((A->B),Vs):-!,ljb(B,[A|Vs]).
ljb(G,Vs1):-
  select((A->B),Vs1,Vs2),
  ljb_imp(A,B,Vs2),
  !,
  ljb(G,[B|Vs2]).

ljb_imp((C->D),B,Vs):-!,ljb((C->D),[(D->B)|Vs]).
ljb_imp(A,_,Vs):-memberchk(A,Vs).


% same, but focus on current goal first
bprove_(T):-ljb_(T,[]).

ljb_(A,Vs):-memberchk(A,Vs),!.
ljb_((A->B),Vs):-!,ljb_(B,[A|Vs]).
ljb_(G,Vs1):-
  select((A->B),Vs1,Vs2),
  ljb_(G,[B|Vs2]),
  ljb_imp_(A,B,Vs2),
  !.

ljb_imp_((C->D),B,Vs):-!,ljb_(D,[C,(D->B)|Vs]).
ljb_imp_(A,_,Vs):-memberchk(A,Vs).




% same but in terms of reverse implication

riprove(A):-riprove(A,[]).

riprove(A,Vs):-memberchk(A,Vs),!.
riprove((B<-A),Vs):-!,riprove(B,[A|Vs]).
riprove(G,Vs1):-
  select((B<-A),Vs1,Vs2),
  riprove_imp(A,B,Vs2),
  !,
  riprove(G,[B|Vs2]).

riprove_imp((D<-C),B,Vs):-!,riprove((D<-C),[(B<-D)|Vs]).
riprove_imp(A,_,Vs):-memberchk(A,Vs).


%% proving (C->D)->B becomes (D->B) -> (C->D)
%% thus we try to prove C->D and assume D->B for that

% variant

bbprove(T):-ljbb(T,[]).

ljbb(A,Vs):-memberchk(A,Vs),!.
ljbb((A->B),Vs):-!,ljbb(B,[A|Vs]).
ljbb(G,Vs1):-
  select((A->B),Vs1,Vs2),
  ljbb((B->G),Vs2),
  ljbb_imp(A,B,Vs2),
  !.

ljbb_imp((C->D),B,Vs):-!,ljbb(((D->B)->(C->D)),Vs).
ljbb_imp(A,_,Vs):-memberchk(A,Vs).

/*
ljbb_add(A,[A|Vs1],[A|Vs2]):-!,ljbb_add(A,Vs1,Vs2).
ljbb_add(A,[(A->B)|Bs],[A,B|BBs]):-!,ljbb_add(A,Bs,BBs).
ljbb_add(A,[B|Vs1],[B|Vs2]):-!,ljbb_add(A,Vs1,Vs2).
ljbb_add(A,[],[A]).
*/


% variant of bprove that produces
% lambda terms as proofs

sprove(T):-sprove(T,_).

sprove(T,X):-ljs(X,T,[]),!.

ljs(X,T):-ljs(X,T,[]),!.

%ljs(X,A,Vs):-ppp((Vs-->X:A)),fail. % just for tracing

ljs(X,A,Vs):-memberchk(X:A,Vs),!. % leaf variable

ljs(l(X,E),(A->B),Vs):-!,ljs(E,B,[X:A|Vs]).  % lambda term

ljs(E,G,Vs1):- 
  member(_:V,Vs1),head_of(V,G),!, % fail if non-tautology
  select(S:(A->B),Vs1,Vs2),   % source of application
  ljs_imp(T,A,B,Vs2),         % target of application
  !,
  ljs(E,G,[a(S,T):B|Vs2]).    % application
  
ljs_imp(l(X,E),(C->D),B,Vs):-!,ljs(E,(C->D),[X:(D->B)|Vs]).
ljs_imp(E,A,_,Vs):-memberchk(E:A,Vs). 
/*  
ljs_imp(E,A,_,Vs):-atomic(A),!,memberchk(E:A,Vs).   
ljs_imp(l(X,E),(C->D),B,Vs):-ljs(E,(C->D),[X:(D->B)|Vs]),
  type_of(l(X,E),T),
  varvars((C->D),TT),
  ppp(l(X,E):has(TT)=inf(T)),
  assertion(TT=T),
  ppp(T),nl.
*/

%ljs_imp(E,A,_,Vs):-atomic(A),!,memberchk(E:A,Vs).   
%ljs_imp(l(X,l(Y,E)),(C->D),B,Vs):-ljs(E,D,[X:C,Y:(D->B)|Vs]). 
 
head_of(_->B,G):-!,head_of(B,G).
head_of(G,G).  
  
eprove(T):-lje(T,[]).

lje(A,Vs):-memberchk(A,Vs),!.
lje((A->B),Vs):-!,lje(B,[A|Vs]). 
lje(G,Vs0):-
  select(T,Vs0,Vs1),head_of(T,G),!,
  select((A->B),[T|Vs1],Vs2),
  lje_imp(A,B,Vs2),
  !,
  lje(G,[B|Vs2]).

lje_imp((C->D),B,Vs):-!,lje((C->D),[(D->B)|Vs]).
lje_imp(A,_,Vs):-atomic(A),memberchk(A,Vs).   


efprove(T):-ljef(T,[]),!.

ljef(A,Vs):-memberchk(A,Vs),!.
ljef((A->B),Vs):-!,ljef(B,[A|Vs]). 
ljef(G,Vs1):-
  member(T,Vs1),head_of(T,G),!,
  select((A->B),Vs1,Vs2),
  ljef_imp(A,B,Vs2),
  !,
  ljef(G,[B|Vs2]).

ljef_imp((C->D),B,Vs):-!,ljef((C->D),[(D->B)|Vs]).
ljef_imp(A,_,Vs):-atomic(A),memberchk(A,Vs).   




% Dyckhoff's LJT - (fig 2 in his paper), 
% enhanced with add_new avoid duplications
   
lprove1(T):-ljt1(T,[]),!.


ljt1(A,Vs):-memberchk(A,Vs),!.     % axiom

ljt1((A->B),Vs1):-
  !,
  add_new(A,Vs1,Vs2),
  ljt1(B,Vs2).                           % => imp 

ljt1(G,Vs1):- %atomic(G),                % imp => 4
  select( ((C->D)->B),Vs1,Vs2),
  add_new((D->B),Vs2,Vs3),
  ljt1((C->D), Vs3),
  !,
  add_new(B,Vs2,Vs4),
  ljt1(G,Vs4).
  
ljt1(G,Vs1):- %atomic(G),                % imp => 1, atom A
  select((A->B),Vs1,Vs2),
  atomic(A),
  memberchk(A,Vs2),
  !,
  add_new(B,Vs2,Vs3),
  ljt1(G,Vs3).

  
% Hudelmaier's O(n*log(n)) space algorithm

nvprove(T):-ljnv(T,[],10000,_).

ljnv(A,Vs)-->{memberchk(A,Vs)},!.
ljnv((A->B),Vs)-->!,ljnv(B,[A|Vs]). 
ljnv(G,Vs1)--> % atomic(G),
  % {member(T,Vs1),eq_head_of(T,G)},!, % slows down
  {select((A->B),Vs1,Vs2)},
  ljnv_imp(A,B,Vs2),
  !,
  ljnv(G,[B|Vs2]).

ljnv_imp((C->D),B,Vs)-->!,newvar(P),ljnv(P,[C,(D->P),(P->B)|Vs]).
ljnv_imp(A,_,Vs)-->{memberchk(A,Vs)}.  

newvar(N,N,SN):-succ(N,SN).

% variant of Hudelmaier's O(n*log(n)) space algorithm
% with sets instead of multisets
nprove(T):-ljn(T,[],1000,_).

ljn(A,Vs)-->{memberchk(A,Vs)},!.
ljn((A->B),Vs1)-->!,{add_new(A,Vs1,Vs2)},ljn(B,Vs2). 
ljn(G,Vs1)--> % atomic(G),
  {select((A->B),Vs1,Vs2)},
  ljn_imp(A,B,Vs2),
  !,
  {add_new(B,Vs2,Vs3)},
  ljn(G,Vs3).

ljn_imp(A,_,Vs)-->{atomic(A),!,memberchk(A,Vs)}.   
ljn_imp((C->D),B,Vs1)-->newvar(P),
   { add_all([
       C,
       (D->P),
       (P->B)
     ],
     Vs1,Vs2)
   },
   ljn(P,Vs2).



add_all([],Ys,Ys):-!.
add_all([X|Xs],Ys,Rs):-
   memberchk(X,Ys),
   !,
   add_all(Xs,Ys,Rs).
add_all([X|Xs],Ys,[X|Rs]):-
add_all(Xs,Ys,Rs).
   





% combines simplicity of bprove and
% dupplicate avoidance with add_new

pprove(T):-ljp(T,[]),!.

ljp(A,Vs):-memberchk(A,Vs),!.
ljp((A->B),Vs1):-!,add_new(A,Vs1,Vs2),ljp(B,Vs2). 
ljp(G,Vs1):- % atomic(G),
  %member(T,Vs1),head_of(T,G),!,
  select((A->B),Vs1,Vs2),
  ljp_imp(A,B,Vs2),
  !,
  add_new(B,Vs2,Vs3),
  ljp(G,Vs3).

ljp_imp(A,_,Vs):-atomic(A),!,memberchk(A,Vs).   
ljp_imp((C->D),B,Vs1):-
   add_new((D->B),Vs1,Vs2),
   ljp((C->D),Vs2).

add_new(X,Xs,Ys):-memberchk(X,Xs),!,Ys=Xs.
add_new(X,Xs,[X|Xs]).
  

% same, with partially evaluated select

mprove(T):-ljm(T,[]),!.

ljm(A,Vs):-memberchk(A,Vs),!.
ljm((A->B),Vs1):-!,
  add_new(A,Vs1,Vs2),
  ljm(B,Vs2). 
ljm(G,Vs):- % atomic(G),
  select_imp(G,Vs,Us,Us).
  
ljm_imp(A,_,Vs):-atomic(A),!,memberchk(A,Vs).   
ljm_imp((C->D),B,Vs1):-
  add_new((D->B),Vs1,Vs2),
  ljm((C->D),Vs2).

select_imp(G,[(A->B)|Vs],Us1,Vs):-
  ljp_imp(A,B,Us1),
  !,
  add_new(B,Us1,Us2),
  ljm(G,Us2).
select_imp(G,[U|Vs],Us,End):-
  select_imp(G,Vs,[U|Us],End).


% trims implications when statically equivalent to A->A
qprove(T0):-
  trimImps(T0,T),
  %ppp(T),
  ljq(T,[]),!.
  
ljq(A,Vs):-memberchk(A,Vs),!.
ljq((A->B),Vs1):-!,add_new(A,Vs1,Vs2),ljq(B,Vs2). 
ljq(G,Vs1):- % atomic(G),
  select(As,Vs1,Vs2),
  imphead(As,B),
  impsel(A,As,Bs),
  ljq_imp(A,B,Vs2),
  !,
  add_new(Bs,Vs2,Vs3),
  ljq(G,Vs3).

ljq_imp(A,_,Vs):-atomic(A),!,memberchk(A,Vs).   
ljq_imp((C->D),B,Vs1):-    
    add_new((D->B),Vs1,Vs2),
    add_new(C,Vs2,Vs3),
    ljq(D,Vs3).

imphead(_->As,H):-!,imphead(As,H).
imphead(H,H).

impsel(A,(A->Bs),Bs).
impsel(A,(B->Bs),(B->Cs)):-impsel(A,Bs,Cs).  


% assumes all hypotheses at once
% while avoiding duplicates

aprove(T):-lja(T,[]),!.

lja(A,Vs):-memberchk(A,Vs),!.
lja(As,Vs1):-As=(_->_),!,
  assume_all(As,B,Vs1,Vs2),
  lja(B,Vs2). 
lja(G,Vs1):- % atomic(G),
  select((A->B),Vs1,Vs2),
  lja_imp(A,B,Vs2),
  !,
  add_new(B,Vs2,Vs3),
  lja(G,Vs3).

lja_imp(A,_,Vs):-atomic(A),!,memberchk(A,Vs).   
lja_imp((C->D),B,Vs1):-
   add_new((D->B),Vs1,Vs2),
   lja((C->D),Vs2).
  
assume_all(A,Last,As,As):-atomic(A),!,Last=A.
assume_all(A->B,Last,As,Bs):-
   memberchk(A,As),
   !,
   assume_all(B,Last,As,Bs).
assume_all(A->B,Last,As,[A|Bs]):-
   assume_all(B,Last,As,Bs).

   flatprove(T):-
%  ppp(T),
  flattenImpl(T,G,Vs),
%  ppp((G:-Vs)),nl,
  ljflat(G,Vs),!.

ljflat(A,Vs):-ppp(ljf=(A:-Vs)),fail.
ljflat(A,Vs):-memberchk(A,Vs),!.
ljflat((A->B),Vs):-!,ljflat(B,[A|Vs]). 
ljflat(G,Vs0):-
  select(X,Vs0,Vs1),head_of(X,G),!,
  select((A->B),[X|Vs1],Vs2),
  ppp(imp=(G:-(A->B))),
  ljflat_imp(A,B,Vs2),
  ppp(out=A+B),
  !,
  ljflat(G,[B|Vs2]).

%ljflat_imp(CD,B,Vs):-ppp(imp=(B:CD:-Vs)),fail.
ljflat_imp((C->D),B,Vs):-!,ljflat((C->D),[(D->B)|Vs]).
ljflat_imp(A,_,Vs):-atomic(A),memberchk(A,Vs).   


% deliberately bad provers
% for testing - randomly succeds or fails

badProve(_) :- badProve(_,_).

badProve(_,_) :- 0 =:= random(2).

looper(_,_):-repeat,sleep(1),fail.

looper(_):-looper(_,_).

% will have false positives

badSolve(A):-badSolve(A,[]).

badSolve(A,Vs):-atomic(A),!,memberchk(A,Vs).
badSolve((A->B),Vs):-badSolve(B,[A|Vs]).
badSolve(_,Vs):-badReduce(Vs).

badReduce([]):-!.
badReduce(Vs):-
  select(V,Vs,NewVs),
  badSolve(V,NewVs),
  badReduce(NewVs).
  
iptest:-
  X= (((c->d)->b) -> (c->d)) ,
  Y= ( (d->b)     -> (c->d)),
  ppp(X<->Y),
  faprove(X<->Y).

