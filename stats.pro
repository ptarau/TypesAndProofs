% helpers, stats, tree display, custom multithreading

:-use_module('third_party/tree_print.pro').
:-include('funcounts.pro').
:-include('nd_threads.pro').
      
% to unary base
n2s(0,0).
n2s(N,s(X)):-N>0,N1 is N-1,n2s(N1,X).

% constructor counters

inits(Cs):-maplist(init,Cs).

init(C):-flag(C,_,0).
inc(C):-flag(C,X,X+1).
inc(C,K):-flag(C,X,X+K).
total(C,R):-flag(C,R,R).


% counts solutions up to M
ncounts(M,Goal):-counts((=),M,Goal).

counts(M,Goal):-counts(n2s,M,Goal).

counts(Transformer,M,Goal,Ts,Rs):-
  findall(T,(
     between(0,M,N),
       call(Transformer,N,S),
       arg(1,Goal,S),
       sols(Goal,T)
       %,write(N-T),write(',')
     ),
  Ts),
   ratios(Ts,Rs).
  
  
counts(Transformer,M,Goal):-
   functor(Goal,F,_),writeln(F:M),
  counts(Transformer,M,Goal,Ts,Rs0),  
  writeln(counts=Ts), 
  maplist(trimdec,Rs0,Rs),
  writeln(ratios=Rs),
  nl.

trimdec(A/B,R):-!,R=A/B.
trimdec(X,Y):-Y is truncate(100*X)/100.

ncvs(M,Goal):-cvs((=),M,Goal).

cvs(Transformer,M,Goal):-
  counts(Transformer,M,Goal,Ts,Rs),  
  plotl(Ts),nl,
  plotl(Rs),nl.


ncvs2(M,Goal1,Goal2):-cvs2((=),M,Goal1,Goal2).

cvs2(Transformer,M,Goal1,Goal2):-
  counts(Transformer,M,Goal1,Ts1,Rs1),  
  counts(Transformer,M,Goal2,Ts2,Rs2), 
  plotls(Ts1,Ts2),nl,
  plotls(Rs1,Rs2),nl.


plotl(Ns):-plotl(0,Ns).

plotl(From,Ns):-
  length(Ns,Len),
  To is From+Len-1,
  numlist(From,To,Is),
  writeln('x,y'),maplist(plotl1,Is,Ns),nl.
  
  
plotls(Ns,Ms):-plotls(0,Ns,Ms).

plotls(From,Ns,Ms):-
  length(Ns,Len),
  To is From+Len-1,
  numlist(From,To,Is),
  writeln('a,b,c'),maplist(plotl2,Is,Ns,Ms),nl.


plotl1(I,N):-number(N),!,write(I),write(','),write(N),nl.
plotl1(_,_).
  
plotl2(I,N,M):-number(N),number(M),!,write(I),write(','),write(N),write(','),write(M),nl.
plotl2(_,_,_).
  
nrelcounts(M,G1,G2):-relcounts((=),M,G1,G2).

relcounts(M,G1,G2):-relcounts(n2s,M,G1,G2).

relcounts(F,M,G1,G2,Cs1,Cs2,Rs):-
  counts(F,M,G1,Cs1,_),
  counts(F,M,G2,Cs2,_),
  ratios2(Cs1,Cs2,Rs).
  
relcounts(F,M,G1,G2):-
  relcounts(F,M,G1,G2,Cs1,Cs2,Rs),
  functor(G1,F1,_),
  functor(G2,F2,_),
  writeln(F1=Cs1),
  writeln(F2=Cs2),
  writeln(F1/F2=Rs),nl.
  
% counts how many times a goal succeeds
sols(Goal, Times) :-
        Counter = counter(0),
        (   Goal,
            arg(1, Counter, N0),
            N is N0 + 1,
            nb_setarg(1, Counter, N),
            fail
        ;   arg(1, Counter, Times)
        ).
  
% benchmarks Goal up to M    
times(M,Goal):-
  between(0,M,N),
  times1(N,Goal).

times1(N,Goal):-n2s(N,S),ntimes1(N,S,Goal).


ntimes(M,Goal):-
 between(0,M,N),
 ntimes1(N,N,Goal).
 
ntimes1(N,S,Goal):-arg(1,Goal,S),
  functor(Goal,F,_),
  time(sols(Goal,Count)),writeln(N:F=count:Count),
  fail.
 
 
time(G,T):-
	get_time(T1),
	once(G),
	get_time(T2),
	T is T2-T1.  
% computes rations between consecutive terms in a sequence
ratios([X|Xs],Rs):-
  map_ratio(Xs,[X|Xs],Rs).

map_ratio([],[_],[]).
map_ratio([X|Xs],[Y|Ys],[R|Rs]):-
  ((Y=:=0)->(R=X/Y);R is X/Y),
  map_ratio(Xs,Ys,Rs).


ratios2([],[],[]).
ratios2([X|Xs],[Y|Ys],[R|Rs]):-
  ((Y=:=0)->(R=X/Y);R is X/Y),
  ratios2(Xs,Ys,Rs).
  
  
% generates and shows terms of size N based on Goal

spp(N,Goal):-n2s(N,S),npp(S,Goal).

npp(S,Goal):-Goal=..[F,S,R|_],nl,writeln(F:S),nl,
   Goal,
   ppt(R),
   fail
; true.
 
show(N,Goal):-
 n2s(N,S),
 nshow(S,Goal).
 
nshow(S,Goal):- 
  functor(Goal,F,_),
  writeln(F),
 
  arg(1,Goal,S),
    Goal,
    show_one(Goal),
  fail.

% shows a term with renamed variables
show_one(Goal):-
  numbervars(Goal,0,_),
  Goal=..[_,_|Xs],
    member(X,Xs),
    writeln(X),
  fail
; nl.

nv(X):-numbervars(X,0,_).

ppp(X):-nv(X),writeln(X),fail.
ppp(_).

% latex qtree printing



qqq(Tree):-acyclic_term(Tree),!,qqq0(Tree).
qqq(Tree):-term_factorized(Tree,Skel,Eqs),
  qqq0(Skel),
  maplist(qqq0,Eqs).

qqq0(T):-to_qtree(T,R),ppp(R),fail;true.

to_qtree(T0,R):-namevars(T0,T),to_qtree(T,Cs,[]),atomic_list_concat(['\\Tree '|Cs],R).

% visits all function syms and constants in aterm
to_qtree(T)-->{
     compound(T),
     T=..[F0|Xs],
     math_wrap(F0,F)    
   },
   ['[.',F,' '],
   to_qtrees(Xs),
   [' ]'].   
to_qtree(T)-->{atom(T);number(T)},['[.',T,' ]'].
to_qtree(T)-->{var(T)},['[.',var_,' ]'].
   
to_qtrees([])-->[].
to_qtrees([X|Xs])-->to_qtree(X),[' '],to_qtrees(Xs).

math_wrap(('->'),R):-!,R='$\\rightarrow$'.
math_wrap(X,X).

sqpp(N,Goal):-n2s(N,S),npp(S,Goal).

qpp(S,Goal):-Goal=..[F,S,R|_],nl,writeln(F:S),nl,
   Goal,
   ppp(R),nl,
   % acyclic_term(R),
   ppt(R),
   qqq(R),
   nl,nl,
   fail
; true.

namevars(T):-namevars(T,T).

namevars(T,NT):-
  copy_term(T,T0),
  numbervars(T0,0,_),
  namevars1(T0,NT).

namevars1('$VAR'(N),S):-!,int2name(N,S).
namevars1(A,R):-atomic(A),!,R=A.
namevars1(A,R):-number(A),!,R=A.
namevars1(T,R):-compound(T),T=..[F|Xs],
  maplist(namevars1,Xs,Ys),
  R=..[F|Ys].


int2name(I,N):-
  atom_codes('XYZUVABCDEFGHIJKLMNOPQRST',Cs),
  nth0(I,Cs,C),
  !,
  atom_codes(N,[C]).
int2name(I,N):-atomic_list_concat(['X',I],N).  
  
s2n(0,0).
s2n(s(X),N):-s2n(X,N1),succ(N1,N).


:-op(800,xfx,(for)).

(I<N) for {Goals}:-
	must_be(integer,N),N1 is N-1,
	must_be(var,I),
	must_be(compound,Goals),
  between(0,N1,I),
    call(Goals), % I should parmeterize goal
	fail
; true.


list(F):-atom(F),!,
  forall(
    between(0,24,N),
    list(F/N)
  ).  
list(F/N):-
 functor(C0,F,N),functor(C,F,N),
 once((
   clause(C0,_,Ref0),
   clause_property(Ref0,file(File)),
   clause_property(Ref0,line_count(Line0))
 )),
 file_name_on_path(File,File0),
 maplist(write,
   ['% ',File0,', line ',Line0,': ',F/N]),nl,nl,
 open(File,read,Stream),
 clause(C,_,Ref),
 clause_property(Ref,line_count(Line)),
 read_source_term_at_location(Stream,
   Term,
   [line(Line),variable_names(Vars)]
 ),
 maplist(call,Vars),
 current_output(Out),
 portray_clause(Out,Term,[quoted(false)]),
 fail
 ;true.
 
ll(FN):-list(FN).

:-op(888,fx,ll).

do(Goal):-
  Goal,
  fail
; true.
