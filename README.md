# TypesAndProofs:

## Type inference algorithms and intuitionistic propositional theorem provers solving type inhabitation problems. Combinatorial and random testers for the provers and type inferencers.


Given the Curry-Howard isomorphism, solving the type inhabitation problem is equivalent to finding propositional implicational intuitionistic tautology proofs.

These tools implement Prolog-based algorithms on the two sides of the *Curry-Howard isomorphism*, including combinatorial and redom testers, centered around:

- generating all candidate type expressions
- generating all simple types
- generating random candidate type expressions
- generating random simple types

### Some theory background needed for grasping what they do:

- elements of sequent calculus and natural deduction
- type inference algorithms for lambda terms
- de Bruijn notation for lambda terms
- beta reduction with de Bruijn indices
- normal forms of lambda terms
- combinatorial generation of trees
- random generation with Boltzmann samplers
- random set-partition generation with urn-algorithms 
- Gentzen's LJ calculus
- Vorobeev-Hudelmayer-Dyckhoff's LJT calculus
- Glivenko's double negation translation
- Fitting's tautology checker


### The provers to be tested and compared are:

- Dyckchoff's program, specialized to implications
- provers derived from the LJT calculus, directly
- provers using Horn clause translations of implicational formulas

The programs are tested with SWI Prolog 7.7.12.

Except for those using the multi-threading the main code, consisting of the the provers and the testers is likely to run on most Prologs.

A Python version of our lightweight implcational logic prover is also included.

