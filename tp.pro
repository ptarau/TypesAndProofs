%:- set_prolog_stack(local,  limit(2000000000)).
%:- set_prolog_stack(global,  limit(2000000000)).

user:prolog_file_type(pro, prolog).

c:-['tp.pro']. % quick iteractive reloader

:-op(1025,fy,do).
:-op(888,fx,ll).
:-op(800,xfx,(for)).
:-op(30,xfx,to).

:- op(100,  fx,  (?) ). 
:- op(525,  fy,  ~ ).    % right associative
:- op(550, xfy,  & ).    % right associative
:- op(575, xfy,  v ).    % right associative
:- op(600, xfx,  <-> ).  % non associative
:- op(800, yfx,  <- ).   % left associative
%:- op(800, xfy,  ->).   % right associative  %%% thiswould  overwride Prolog's ->
               
:-include('horn_flattener.pro').

:-include('stats.pro'). % tools, including statistical and displayers

:-include('allTrees.pro').
:-include('RemyR.pro').  % random binary tree, Knuth's algorithm R
%:-include('RemyP.pro'). % declarative alternative

:-include('allPartitions.pro'). % partitions of set of given size
:-include('ranPartition.pro'). % random partitions

:-include('betaReducer.pro'). % reducers for lambda terms in de Bruijn from

:-include('allTypedNFs.pro'). % generator for simply typed normal forms
:-include('ranNormalForms.pro'). % random normal form generator (Boltzmann)

:-include('ranFullFormulas.pro'). % random full intuitionistic formulas

:-include('allFormulas.pro').

:-include('ranImpFormulas.pro'). % random implicational formulas

:-include('iProvers.pro'). % provers of implicational intuitionistic logic

:-include('fullProvers.pro'). % provers hadling all connectives

:-include('parProvers.pro'). % other provers

:-include('hProvers.pro'). % provers with nested Horn clauses

:-include('nProvers.pro'). % provers with nested Horn clauses for &,<->,->,~

:-include('cProvers.pro'). % provers  for classical propostional logic

:-include('oProvers.pro'). % other provers

:-include('toHorn.pro'). % translators to/from embeded Horn Clauses

:-include('parImpFormulas.pro'). % parallel variants of generators for  implicational tautologies

:-include('parTypedNFs.pro'). % parallel variants of typed normal form generators

:-include('old_abducers.pro').
:-include('abducers.pro').

:-include('inputTransformers.pro').

:-include('mints.pro').

:-include('hardening.pro').

:-include('testGenerators.pro').

:-include('testProvers.pro').

%:-include('canonical_horn.pro').

:-include('circSynt.pro').

:-include('harrop.pro').

:-include('ieltp.pro'). % experiments with embedding an epistemic operator

:-include('treeRanking.pro').

:-include('inhab.pro').

:-include('tools.pro').

:-include('counts.pro').

:-include('benchmarks.pro').
:-include('bm.pro').

:-include('tester.pro'). % soup of testing and benchmarcking programs

:-include('printers.pro'). % shows trees and other things in ASCII and LaTeX

:-include('encoders.pro').

:-include('hdprovers.pro').

%:-include('third_party/dyckhoff.pro'). % implicational variant of Roy Dyckhoff's prover 

:-include('third_party/dyckhoff_orig.pro'). % implicational variant of Roy Dyckhoff's prover  

:-include('third_party/g4ip.pro').


:-include('third_party/mleantap.pro').
%:-include('third_party/ileantap.pro').

:-include('third_party/ileansep.pro').
:-include('third_party/ileancop.pro').

%:-include('third_party/fitting.pro'). % implicational variant of M. Fitting's prover
:-include('third_party/fitting_orig.pro'). %  M. Fitting's prover (full IPL)

:-include('fix_iltp.pro').

:-include('primes.pro').

:-include('uhfs.pro').

:-include('hhfs.pro').

:-include('fc.pro').

