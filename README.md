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
?- bprove(a->b->a).
true.
```

```
?- bprove((a->b)->a).
false.
```

See a lot of examples of use in file *tester.pro*
Generic benchmarking code is in bm.pro.

Given the Curry-Howard isomorphism, solving the type inhabitation problem is equivalent to finding propositional implicational intuitionistic tautology proofs.

These tools implement Prolog-based algorithms on the two sides of the *Curry-Howard isomorphism*, including combinatorial and random testers, centered around:

- generating all candidate type expressions
- generating all simple types
- generating random candidate type expressions
- generating random simple types

### Some theory background needed for grasping what they do:

- elements of sequent calculus and natural deduction
- type inference algorithms for lambda terms
- normal forms of lambda terms
- combinatorial generation of trees, set partitions
- random set-partition generation with urn-algorithms 
- random term generation with Boltzmann samplers
- Gentzen's LJ calculus
- Vorobiev-Hudelmaier-Dyckhoff's LJT calculus
- Glivenko's double negation translation
- Fitting's classical tautology checker
- de Bruijn notation for lambda terms
- beta reduction with de Bruijn indices


### The provers to be tested and compared are:

- Dyckchoff's program, specialized to implications
- provers derived from the LJT calculus, directly
- provers using Horn clause translations of implicational formulas
- classical Provers, via Glivenko's translation
- full intuitionistic propositional provers

The programs are tested with SWI Prolog 7.7.12.

Except for those using SWI-Prolog's multi-threading the code, the provers and the testers are likely to run on most Prologs.

This work-in-progress [paper](https://github.com/ptarau/TypesAndProofs/tree/master/docs/iprover.pdf) documents some key components of this code repository.

For comparing with other provers, we have ported the propositional subset of the [ILTP library]( http://www.iltp.de/ ) to SWI-Prolog and uniformized notation of some third party provers.

Try:

```
?-load_probs1. % our prover
?-load_probs2.
...
?-load_probsN.
```

for their respective performance.

So far, our faprove/1 and ff_prove/1 full propositional IL provers are the only ones passing all correctness tests as well as avoidance of stack overflow errors.
All provers can be tested at various timout levels by changing max_time/1 in file *tester.pro*

