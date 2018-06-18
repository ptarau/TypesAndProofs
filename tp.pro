%:- set_prolog_stack(local,  limit(2000000000)).
%:- set_prolog_stack(global,  limit(2000000000)).

c:-['tp.pro']. % quick iteractive reloader

:-op(1025,fy,do).
:-op(888,fx,ll).
:-op(800,xfx,(for)).
:-op(30,xfx,to).

:- op(425,  fy,  ~ ).
:- op(450, yfx,  & ).    % left associative
:- op(450, yfx,  v ).    % left associative
:- op(500, xfx,  <-> ).  % non associative
%:- op(500, xfy,  ->).    % right associative  
%%% WARNING, this overwrites Prolog's ->
                
                
:-include('stats.pro'). % tools, including statistical and displayers

:-include('allTrees.pro').
:-include('remyR.pro').  % random binary tree, Knuth's algorithm R
%:-include('remyP.pro'). % declarative alternative

:-include('allPartitions.pro'). % partitions of set of given size
:-include('ranPartition.pro'). % random partitions

:-include('betaReducer.pro'). % reducers for lambda terms in de Bruijn from

:-include('allTypedNFs.pro'). % generator for simply typed normal forms
:-include('ranNormalForms.pro'). % random normal form generator (Boltzmann)

:-include('allImpFormulas.pro').

:-include('ranImpFormulas.pro'). % random implicational formulas

:-include('iProvers.pro'). % provers of implicational intuitionistic logic

:-include('fullProvers.pro'). % provers hadling all connectives

:-include('hProvers.pro'). % other provers with embedded Horn clauses

:-include('cProvers.pro'). % provers  for classical logic

:-include('toHorn.pro'). % translators to/from embeded Horn Clauses

:-include('parImpFormulas.pro'). % parallel variants of generators for  implicational tautologies

:-include('parTypedNFs.pro'). % parallel variants of typed normal form generators

:-include('abducers.pro').

:-include('inputTransformers.pro').

:-include('testGenerators.pro').

:-include('testProvers.pro').

:-include('tools.pro').

:-include('benchmarks.pro').
:-include('bm.pro').

:-include('tester.pro'). % soup of testing and benchmarcking programs

:-include('printers.pro'). % shows trees and other things in ASCII and LaTeX


%:-include('third_party/dyckhoff.pro'). % implicational variant of Roy Dyckhoff's prover 

:-include('third_party/dyckhoff_orig.pro'). % implicational variant of Roy Dyckhoff's prover  

:-include('third_party/g4ip.pro').


:-include('third_party/ileantap.pro').

:-include('third_party/fitting.pro'). % implicational variant of M. Fitting's prover

:-include('fix_iltp.pro').

