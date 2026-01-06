:-op(500,yfx,('^')).
:-op(500,xfx,('=>')).
:-op(500,xfx,('<=')).
:-op(200,fx,('~')).

if_then_else(X,Y,Z,R):-
  bit(X),bit(Y),bit(Z),
  ( X==1->R=Y
  ; R=Z
  ).

bit(0).
bit(1).

all_ones_mask(NbOfBits,Mask):-Mask is (1<<(1<<NbOfBits))-1. 

var_to_bitstring_int(NbOfBits,K,Xk):-
  all_ones_mask(NbOfBits,Mask),
  NK is NbOfBits-(K+1),
  D is (1<<(1<<NK))+1,
  Xk is Mask//D.

vars_to_bitstring_ints(NbOfBits,Vs):-
  vars_to_bitstring_ints(NbOfBits,0,NbOfBits,Vs).

vars_to_bitstring_ints(_,N,N,[]).
vars_to_bitstring_ints(NbOfBits,N1,N2,[X|Xs]):-
  N1<NbOfBits,
  N is N1+1,
  var_to_bitstring_int(NbOfBits,N1,X),
  vars_to_bitstring_ints(NbOfBits,N,N2,Xs).

synthesize(NV,MG,Fs,Cs, TTs,  Xs,Gs,Ys):-
  initInputs(NV,Cs,M,Vs,Is),
  try_gates(MG,M,Fs,TTs,Is,Gs0,_,[]),
  !,
  symplify_dag(NV,Cs,Vs,Gs0, Xs,TTs,Ys, Gs).

try_gates(NG,M,Fs,TTs,Is, Gs,Os,NewTTs):-
  trim_tts(TTs,Is,_SolvedTTs,UnSolvedTTs),
  generate_gates(NG,M,Fs,UnSolvedTTs,Is, Gs,Os,NewTTs).

% generates gates
generate_gates(_,_,_,TTs,Is, Is,Is,TTs).
generate_gates(NG,M,Fs,TTs,Is, [G|Gs],[VO|Os],NewTTs):-
  NG>0,NG1 is NG-1,
  generate_gates(NG1,M,Fs,TTs,Is, Gs,Os,OldTTs),
  newGate(Fs,M,Os,G,VO),
  check_progress(VO,OldTTs,NewTTs).

 
newGate(Fs,M,Os,G,VO):-
  member(F,Fs),
  newGateByArity(F,M,Os,G,VO),
  \+ member(VO,Os).

syn(E):-syn([<, =>],[0,1],E).

syn(Fs,E):-syn(Fs,[],E).

syn(Fs,Cs,E):-expr2tt(E,NV:TT),syn(Fs,Cs,NV,TT).

syn(Fs,Cs,NV,TT):-
  init_tts(NV,TT, MG,TTs),
  showsyn(NV,MG,Fs,Cs,TTs).

showsyn(NV,MG,Fs,Cs,TTs):-
  portray_clause(syn(NV,MG,Fs,Cs,TTs)),
  show_tts(NV,TTs),
  statistics(runtime,[T1,_]),
  synthesize(NV,MG,Fs,Cs,TTs, Xs,Gs,Ys),
  R=(Xs:Gs=Ys:TTs),
  !,
  statistics(runtime,[T2,_]),
  portray_clause(R),
  T is T2-T1,
  write(time_ms=T),nl,
  fail. % to avoid unnecessary bindings to be shown

synexp(E,MG,Fs,Cs, NV:TTs, Xs,Gs,Ys):-
  expr2tt(E,NV:TTs),
  synthesize(NV,MG,Fs,Cs, TTs,  Xs,Gs,Ys).

% enumerates circuits in increasing order
enumerateCircuits(_MG,_M,_Fs,Is, Is,[TT]):-
  % when outputs connect directly to inputs
  member(TT,Is).
enumerateCircuits(MG,M,Fs,Is, Gs,Os):-
  % when gates connect inputs to outputs
  generate_gates(MG,M,Fs,[_AnyTT],Is, Gs,Os,_). 

%-- bitstring int operations on boolean functions
% can also be seen as f:[0..M]x[0..M]->[0..M]
% or f:[0..3]->[0..1] or f<-[0..15] using their tt

applyF('~',M,A,R):-R is xor(M,A).

applyF('nand',M,X1,X2,X3):-X3 is xor(M,/\(X1,X2)).
applyF('nor',M,X1,X2,X3):-X3 is xor(M,\/(X1,X2)).
applyF('<',_,X1,X2,X3):-X3 is xor(X1,\/(X1,X2)). %k
applyF('>',_,X1,X2,X3):-X3 is xor(X1,/\(X1,X2)). %k
applyF('=>',M,X1,X2,X3):-X3 is \/(xor(M,X1),X2).
applyF('<=',M,X1,X2,X3):-X3 is \/(X1,xor(M,X2)).
applyF('*',_,X1,X2,X3):-X3 is /\(X1,X2). %k
applyF('+',_,X1,X2,X3):-X3 is \/(X1,X2). %k
applyF('=',M,X1,X2,X3):-X3 is xor(M,xor(X1,X2)).
applyF('^',_,X1,X2,X3):-X3 is xor(X1,X2). %k

applyF('head',_,X1,_,X3):-X3 is X1.
applyF('tail',_,_,X2,X3):-X3 is X2.
applyF('nhead',M,X1,_,X3):-X3 is xor(M,X1).
applyF('ntail',M,_,X2,X3):-X3 is xor(M,X2).

applyF('zero',_,_,_,0).
applyF('one',M,_,_,M).

applyF('ite',_M,A,B,C,R):-D is xor(B,C),E is /\(D,A),R is xor(E,C).


% --------- pre-synthesis initializer ----------------------
% input initializer/generator
% precomputes bitvector representations of variables and constants
initInputs(NV,Cs, Mask,Vs,Is):-
 init_vars(NV,Mask,Vs),
 init_consts(Cs,Mask,ICs),
 append(ICs,Vs,Is).
    
% constant mapping
const(0,_M,0). % false=0
const(1,M,M).  % true=M (Mask)

% precompute constants
init_consts([],_,[]).
init_consts([C|Cs],M,[VC|ICs]):-
  const(C,M,VC),
  init_consts(Cs,M,ICs).

% precomputes bitvector values for variables
init_vars(NV,Mask,VPairs):-
  all_ones_mask(NV,Mask),
  vars_to_bitstring_ints(NV,VPairs).

% ---------- pre-synthesis converters ---------  
% converts expressions and truth table notations 
% to canonical truth table form
expr2tt(NV:TT,NV:TTs):-integer(NV),!,to_list(TT,TTs).
expr2tt((Vs:E),NV:TT):-!,to_list(E,Es),eval_expr(Vs,Es,NV,TT).
expr2tt(E,NV:TT):-
  Vs=[],
  eval_expr(Vs,E,NV,TT).

% expression evaluator - supports all 16 binary ops, ~,ite
eval_expr(Vs0,E,NV,I):-
  to_list(E,Es0),
  copy_term(Vs0+Es0,Vs+Es),
  numbervars(Vs+Es,0,NV),
  all_ones_mask(NV,M),
  mapeval(Es,NV,M,R),
  !,
  R=I.

% evaluates a list of expressions
mapeval([],_,_,[]).
mapeval([E|Es],NV,M,[R|Rs]):-
   eval_one(E,NV,M,R),
   mapeval(Es,NV,M,Rs).

% evaluates one expression
eval_one(E,_,M,I):-integer(E),!,
  const(E,M,I).
eval_one('$VAR'(K),NV,_M,I):-!,
  var_to_bitstring_int(NV,K,I).
eval_one(E,NV,M,I):-functor(E,F,2),!,
  arg(1,E,X),arg(2,E,Y),
  eval_one(X,NV,M,A),eval_one(Y,NV,M,B),
  applyF(F,M,A,B,I).
eval_one(~(E),NV,M,I):-!,
  eval_one(E,NV,M,A),
  I is xor(M,A).
eval_one(ite(X,Y,Z),NV,M,I):-!,
  eval_one(((X*Y)+(~(X)*Z)),NV,M,I).
eval_one(mux(X,Y,Z),NV,M,I):-!,
  eval_one(((~(X)*Y)+(X*Z)),NV,M,I).

% ------- post-synthesis converters -----
% canonical form converter - for more readable gates
symplify_dag(NV,Cs,VVs,Gs, Vs,Os,Ys,NewGs):-
  simplify_consts(NV,Cs,D),
  simplify_list(VVs,Vs,D),
  simplify_list(Os,Ys,D),
  reverse(Gs,Rs),
  simplify_gates(Rs,NewGs,D).

simplify_consts(NV,Cs,D):-
  all_ones_mask(NV,M),
  init_consts(Cs,M,As),
  simplify_list(As,Cs,D).

simplify_list([],[],_).
simplify_list([C|Cs],[X|Xs],D):-to_var(C,X,D),simplify_list(Cs,Xs,D).

simplify_gates([],[],_).
simplify_gates([G|Gs],[T|Ts],D):-
  G=..[g,Op|As],
  !,
  simplify_list(As,Xs,D),
  T=..[Op|Xs],
  simplify_gates(Gs,Ts,D).
simplify_gates([_C|Gs],Ts,D):-
  simplify_gates(Gs,Ts,D).
  
to_var(C,X,D):-member(v(X,C),D),!.

% ---- post-synthesis result formatters

% prints the truth table(s) associated
% to a (list) of formulae or integer tts
tts(EorEs):-
  expr2tt(EorEs,NV:TTs),
  show_tts(NV,TTs).

% prints out a list of NV variable truth tables TTs
show_tts(NV,TTs):-
  ( member(TT,TTs),
      show_tt(NV,TT),
    fail
  ; true
  ).


% prints out a truth table  
show_tt(NV,Int):-
  show_tt(NV,Int,BsV),
  write(BsV),nl,
  fail
; nl.
show_tt(NV,Int,Bs:V):-
  findall(Bs,tt_line(NV,Bs),Bss),
  T=..[tt|Bss],
  functor(T,_,N),
  between(1,N,I),
  arg(I,T,Bs),
  I1 is N-I,
  getbit(Int,I1,V).

% prints out a line in a truth table  
tt_line(0,[]).
  tt_line(N,[B|Bs]):-N>0,N1 is N-1,(B=0;B=1),tt_line(N1,Bs).
  
% gets a the state of Bit position in Int, returned as Val in {0,1}
getbit(Int,Bit,Val):- Val is (/\(Int,(1<<Bit)))>>Bit.

% default truth table initialization
init_tts(NV,TT, MG,TTs):-
  to_list(TT,TTs),
  length(TTs,L),
  MG0 is L*NV*(1<<NV),
  (MG0<6->MG=6;MG=MG0),
  write(['TTs'=TTs,'MG'=MG]),nl.

% ensures non-list arguments are lifted to lists
to_list(Es,R):-nonvar(Es),Es=[_|_],!,R=Es.
to_list(Es,R):-nonvar(Es),Es=[],!,R=[].
to_list(E,[E]).

% arity 1 gate - negation
newGateByArity(F,M,Os,g(F,VK,VO),VO):-F='~',!,
  member(VK,Os),
    applyF(F,M,VK,VO).
% arity 3 gate - if-then-else 
newGateByArity(F,M,Os,g(F,VK,VI,VJ,VO),VO):-F=ite,!,
  member(VK,Os),
    member(VI,Os),
      member(VJ,Os),
       applyF(F,M,VK,VI,VJ,VO). 
% 16 arity 2 gates
newGateByArity(F,M,Os,g(F,VI,VJ,VO),VO):-
    member(VI,Os),
      member(VJ,Os),
       applyF(F,M,VI,VJ,VO).

% trims truth table lists of plain inputs - they need no search         
trim_tts(TTs,Is,SolvedTTs,UnSolvedTTs):-
  findall(X,(member(X,TTs),member(X,Is)),SolvedTTs),
  findall(X,(member(X,TTs),\+member(X,Is)),UnSolvedTTs).

%check_progress(VO,Os,_OldTTs,_NewTTs):-member(VO,Os),!,fail.
check_progress(VO,OldTTs,NewTTs):-pick(VO,OldTTs,More),!,NewTTs=More.
check_progress(_VO,TTs,TTs).

% selects/inserts a value from/to a list
pick(X,[X|Xs],Xs).
pick(X,[Y|Xs],[Y|Ys]):-pick(X,Xs,Ys).

