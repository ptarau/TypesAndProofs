% Intuitionistic theorem prover
% Written by Roy Dyckhoff, Summer 1991    
% Modified to use the LWB syntax  Summer 1997
% and simplified in various ways...

% runs underSWI Prolog
% for other Prologs, maybe a different way of timing is required
% and maybe "append" needs to be defined explicitly.

% Incorporates the Vorob'ev-Hudelmaier etc calculus (I call it LJT)
% See RD's paper in JSL 1992:
% "Contraction-free calculi for intuitionistic logic"

% Torkel Franzen (at SICS) gave me good ideas about how to write this 
% properly, taking account of first-argument indexing

% and I learnt a trick or two from Neil Tennant's "Autologic" book.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A is formula to prove
% TIme is in millisec

% reduced to implicational fragment, remove heuristics,
% tested with all tautologies havbing proofs of size less then 13
% ensured SWI-Prolog compatibility - Paul tarau, 2018



prove( A,true ) :- provable(A), !.
prove( _,false ). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SYNTAX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% all atoms MUST be of form pv(X) for some X

% inspv(A, NewA) ensures that NewA has all the atoms with pv/1 as an 
% explicit constructor, making the clauses more deterministic

inspv( A -> B, A1 -> B1) :- !, inspv(A, A1), inspv(B, B1).
inspv( true, true ) :- !.           % true not an atom 
inspv( false, false ) :- !.         % false not an atom 

inspv( A, pv(A) ).                  % when all else fails 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UTILITIES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AtomImps is an ordered list PL of pairs (A,LA) where LA is a list
% and A is an atom
% lists PL are ordered by @< relation on first component
% LA is list of formulae B 
% No attempt to avoid repetition in LA.

% meaning( []) = true
% meaning( ( Pair | Rest|) = meaning( Pair) & meaning(Rest)
% meaning( (A,Bs) ) = A -> conjunction(Bs)

% extract( AtomImps, A,  Bs, RestImps)
% called with AtomImps and A known (an Atom); returns Bs and RestImps
% thus  meaning(AtomImps) ==  meaning(A->Bs) & meaning(RestImps)

extract( [ ], _, [], []).
extract( [  Pair | AtomImps], A,  Bs, [ Pair | RestImps ] ) :- 
  Pair = (A1, _), 
  A @> A1,                                    % still worth looking
  extract(AtomImps, A, Bs, RestImps). 
extract( [ Pair | RestImps], A,  Bs, RestImps) :-
  Pair = (A, Bs).                             % Found it
extract( AtomImps, A,  Bs, AtomImps) :-
  AtomImps = [ (A1, _) | _ ],  
  A @< A1,                                    % gone too far
  Bs = [].                           % Not there, return empty list

% insert( AtomImps, A -> B, AtomImps1)
% where A is an atom

insert( [], A -> B,  [ (A, [B]) ] ).
insert( [  Pair  |AtomImps], A -> B, [ Pair | AtomImps2 ] ) :-
   Pair = (A1, _), 
   A @> A1,   % Needs to go further in
   insert(AtomImps, A -> B, AtomImps2).
insert( [ (A, Bs)|AtomImps], A -> B, [(A,[B|Bs])|AtomImps] ).  % Here!
insert( [ Pair   |AtomImps], A -> B, [(A,[B]),Pair|AtomImps] ) :-
   Pair = (A1, _), 
   A @< A1.   % Needs to go before Pair

% ===============================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ==============================
provable( A ) :-    
  inspv(A, A1), 
  redsucc(A1, [], [], []), !.

% ==============================
 

% AtomImps is a list as described above
% NestImps is a list of formulae of the form (C->D)->B
% where if B = false, then D=false
% Atoms is a list of atoms

redant( [ ], AtomImps, NestImps, Atoms,  G) :-
  redsucc(G, AtomImps, NestImps, Atoms).
redant( [ A | L], AtomImps, NestImps, Atoms, G) :-  
  redant1(A, L, AtomImps, NestImps, Atoms, G).

% ------------------------------

redant1(  pv(A), _, _, _, _, pv(A)  ).

redant1( pv(A),  L, AtomImps, NestImps, Atoms, G) :- 
   \+ G = pv(A) ,
  extract(AtomImps, pv(A), Bs, RestAtomImps), 
  append(Bs, L, BsL), 
  redant( BsL, RestAtomImps, NestImps, [pv(A)|Atoms], G ).

redant1( true, L, AtomImps, NestImps, Atoms, G ) :- 
  redant( L, AtomImps, NestImps, Atoms, G).

redant1( false, _, _, _, _, _ ).


redant1( A -> B, L, AtomImps, NestImps, Atoms, G ) :-
  redantimp(A, B, L, AtomImps, NestImps, Atoms, G). 


% ------------------------------
% redantimp

redantimp( pv(A), B, L, AtomImps, NestImps, Atoms, G ) :-
  redantimpatom( pv(A), B, L, AtomImps, NestImps, Atoms, G).

redantimp( true, B, L, AtomImps, NestImps, Atoms, G ) :- 
  redant1(B, L, AtomImps, NestImps, Atoms, G).

redantimp( false, _, L, AtomImps, NestImps, Atoms, G ) :- 
  redant( L, AtomImps, NestImps, Atoms, G).

redantimp( C -> D, B, L, AtomImps, NestImps, Atoms,  G ) :-
  redantimpimp( C, D, B, L, AtomImps, NestImps, Atoms, G).

% ------------------------------
% redantimpimp 


redantimpimp( C, false, false, L, AtomImps, NestImps, Atoms, G ) :- 
  redant(L, AtomImps, [ (C -> false) -> false | NestImps ], Atoms, G).

    % next clause exploits ~(C->D) eq  (~~C & ~D)
    % which isn't helpful when D = false


redantimpimp( C, D, false, L, AtomImps, NestImps, Atoms, G) :-
  \+(D = false),  
  redantimpimp( C, false, false,  [ D -> false | L] , AtomImps, NestImps, Atoms, G).

redantimpimp( C, D, B, L, AtomImps, NestImps, Atoms, G) :- 
  \+ (B = false), 
  redant(L, AtomImps, [ (C -> D) -> B | NestImps ], Atoms, G).


%-----------------------------------------------
%redantimpatom

redantimpatom( A, B, L, AtomImps, NestImps, Atoms, G ) :-
  on3(A, Atoms, Res), 
  redantimpatomres(Res,  A, B, L, AtomImps, NestImps, Atoms, G ).

redantimpatomres( true, _, B, L, AtomImps, NestImps, Atoms, G ) :- 
  % A is on Atoms: reduce using rule ->L1
  redant1(B, L, AtomImps, NestImps, Atoms, G).

redantimpatomres( false,  A, B, L, AtomImps, NestImps, Atoms, G ) :-
  % A is atom but not on Atoms, add A->B to AtomImps
  insert(AtomImps, A -> B, AtomImps1), 
  redant(L, AtomImps1, NestImps, Atoms, G).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% redsucc(Goal, AtomImps, NestImps, Atoms)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

redsucc( true, _, _, _  ).

redsucc( pv(A), AtomImps, NestImps, Atoms ) :- 
  on3(pv(A), Atoms,  Res),
  redsuccpv(Res,  pv(A), AtomImps, NestImps, Atoms).
  
redsucc( A -> B, AtomImps, NestImps, Atoms ) :-
  redant1(A, [], AtomImps, NestImps, Atoms, B).

  
redsuccpv( true, _, _, _, _ ).
redsuccpv(false,  pv(A), AtomImps, NestImps, Atoms ) :-
  posin(pv(A), AtomImps, NestImps),                     % MAJOR PRUNING HERE
  redsucc_choice( pv(A), AtomImps, NestImps, Atoms).
 
  
%----------------------------------------

% Now we have the hard part; maybe lots of formulae 
% of form (C->D)->B  in Imps to choose from!
% Which one to take first? We need some heuristics:
% first we take those where the minor premiss is trivial;
% next we take those where B is an atom and B->G is in AtomImps


redsucc_choice( G, AtomImps, NestImps, Atoms ) :- 
  select(( (C->D) -> B), NestImps, Rest),
  redant3(C, D, B, AtomImps, Rest, Atoms, G),
  !.  % MAIN CUT here

redant3( C, D, B, AtomImps, NestImps, Atoms, G ) :-
  redant1(D->B, [], AtomImps, NestImps, Atoms, C->D), %  Left Premiss
  !,  % CUT here; rule is semi-invertible
  redant1(B, [], AtomImps, NestImps, Atoms, G).       % Right Premiss

% helpers

on3( A, L, Res ) :-memberchk(A, L),!, Res = true.
on3(_, _, false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pruning utilities

posin( G, AtomImps, NestImps ) :-
  % G occurs strictly positively on LHS
  posin1( AtomImps, G ), 
  ! 
;
  posin2( NestImps, G ). 

posin1( [ ( _, Bs ) | AtomImps ], G ) :-
  posin2( Bs, G ), 
  ! 
; 
  posin1( AtomImps, G ).
  
posin2( [ B | Bs ], G ) :-
  posin3( B,G ), 
  ! 
;
  posin2( Bs, G).

posin3( _ -> B, G ) :-
  posin3(B,G).
 
posin3( false, _ ).
posin3( G, G ).


