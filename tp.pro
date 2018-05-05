c:-['tp.pro']. % quick iteractive reloader

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

:-include('classTaut.pro'). % basic classical tautology testers

:-include('iProvers.pro'). % provers of implicational intuitionistic logic

:-include('hProvers.pro'). % other provers with embedded Horn clauses

:-include('aProvers.pro'). % other provers (including for classical logic)

:-include('toHorn.pro'). % translators to/from embeded Horn Clauses

:-include('parProgs.pro'). % parallel variants of some of the programs

:-include('inputTransformers.pro').

:-include('testGenerators.pro').

:-include('testProvers.pro').

:-include('benchmarks.pro').

:-include('tester.pro'). % soup of testing and benchmarcking programs

:-include('third_party/dyckhoff.pro'). % implicational variant of Roy Dyckhoff's prover  

:-include('third_party/fitting.pro'). % implicational variant of M. Fitting's prover


