# TypesAndProofs:

## Type inference algorithms and intuitionistic propositional theorem provers solving type inhabitation problems. Combinatorial and random testers for the provers and type inferencers.

Type

```
$ go
```

or

```
swipl -s tp.pro
```
and then something like

```
?- pprove(a->b->a).
true.
```

```
?- pprove((a->b)->a).
false.
```

See a lot of examples of use in file *tester.pro*

Given the Curry-Howard isomorphism, solving the type inhabitation problem is equivalent to finding propositional implicational intuitionistic tautology proofs.

These tools implement Prolog-based algorithms on the two sides of the *Curry-Howard isomorphism*, including combinatorial and random testers, centered around:

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
- combinatorial generation of trees, set partitions
- random set-partition generation with urn-algorithms 
- random term generation with Boltzmann samplers
- Gentzen's LJ calculus
- Vorobeev-Hudelmayer-Dyckhoff's LJT calculus
- Glivenko's double negation translation
- Fitting's classical tautology checker


### The provers to be tested and compared are:

- Dyckchoff's program, specialized to implications
- provers derived from the LJT calculus, directly
- provers using Horn clause translations of implicational formulas

The programs are tested with SWI Prolog 7.7.12.

Except for those using SWI-Prolog's multi-threading the code, the provers and the testers are likely to run on most Prologs.

A Python version of one of our lightweight implicational logic provers is also included.

This work-in-progress [paper](https://github.com/ptarau/TypesAndProofs/tree/master/docs/iprover.pdf) documents some key components of this code repository.