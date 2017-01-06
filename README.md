# quantum-algorithms
[![Build Status](https://travis-ci.org/f-stibane/quantum-algorithms.svg?branch=master)](https://travis-ci.org/f-stibane/quantum-algorithms)

## About
This project aims to give an introduction to CS people into Quantum Algorithms or Quantum Computing in general.
That will be achieved with these (planned) features:
* A set of DSLs to write Quantum Algorithms that are close to those one would write on a blackboard
* An alternative graphical representation of Quantum Algorithms 
* Calculation of states, mutated by applying Operators
* Implementations of the most common Operators
* Easy creation of custom Operators
* A web GUI in which to write, read and debug Quantum Algorithms
* A set of examples of commonly known Quantum Algorithms

## What's working right now
At the moment most of the work is put into a rewrite to enable concurrent work, mainly through modularization.
With the library on the master branch you can
* Write, run and debug Quantum Algorithms using an internal DSL (see 'Run teleportation example')

## Installation:
```
sbt clean publish-local
```

## Run teleportation example:
```
sbt 'examples/runMain qa.examples.Teleportation'
```
