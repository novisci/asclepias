# maybe-toxic-tutorial
A play on "asclepias", a genus of milkweeds. They are toxic to most animals yet are named for the Greek god of healing. %-}

Maybe also is an important data type in Haskell.

Just Learn or Nothing

## Goals

Look at examples, do some exercises based on those examples, and run the tests to see if your output was correct. The examples and exercises are meant to be fiddled with interactively in ghci.

This just cribs the learning-loop from exercism.org or the rustlings course.

The tutorial will be broken into conceptual blocks. Each concept will have three files associated with it

- `src/{concept_name}Examples.hs`
- `src/{concept_name}Exercises.hs`
- `test/{concept_name}Tests.hs`

## A work in progress
A rough roadmap for modules to add is below. If you think this general idea is useful and want to add or modify content --- go for it! Doing so is itself a good learning experience and has been for me.

* [ ] Type basic
		- Reading and using type declarations in type signatures
		- Typeclasses
		- Implementing a type as an instance of a typeclass
		- Maybe
* [ ] Functional programming basics
		- lambda functions as the building block
		- currying
		- folds and maps
		- a functional state of mind
* [ ] Interval algebra
    - creating intervals
		- relating intervals
		- modifying intervals
		- defining our own intervals
		- defining structures that contain intervals
* [ ] asclepias basics
* [ ] asclepias extended

## How to use
For now this will live in asclepias.

1. clone the asclepias repo's `tutorial` branch

2. you might need to run `cabal install interval-algebra --lib` to get ghci to load the examples depending on that package

3. open an examples file, e.g. TypeExamples.hs

4. side-by-side (ideally) load the module in ghci by 
    i. running `cabal exec -- ghci` to ensure dependencies are loaded
		ii. in typing `:l path_to_module/TypeExamples.hs`. For example, if `maybe-toxic-tutorial` is my current working directory I would run the following in ghci `:l src/TypeExamples.hs`

5. look at the examples, run the code in ghci, try the exercises.

## How to contribute
Any old way you want to. But if you want some ideas:

- add/modify examples in existing tutorials
- add/modify exercises in existing tutorials
- build the testing framework
- add tests (when the testing framework is built out, probably best to add these when you add exercises)
- add commentary to anything you felt was unclear
- add tutorials, eg for functional programming basics
- modify this README, especially the task list above
