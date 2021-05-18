# Project Asclepias

_Asclepias (n)_:

1. The genus of North American milkweeds, named after Linnaeus after the greek god of healing, Asclepius.
2. A language and software project for defining and generating epidemiological cohorts from streams of events.

## Current status

The initial versions of `hasklepias` will focus on the ability to derive features from a sorted collection of events. As of the initial release (`v0.1.0`), developers can experiment with feature definitions (see below and the `examples` directory).

## Getting started

The official implementation of Asclepias is the embedded domain specific language (eDSL) provided by the `hasklepias` [Haskell](https://www.haskell.org/) library. To get started then, you'll need to install the Haskell toolchain, especially the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) (GHC) and the building and packaging system [cabal](https://www.haskell.org/cabal/), for which you can use the [`ghcup` utility](https://www.haskell.org/ghcup/).

You can use any development environment you chose, but for maximum coding pleasure, it is highly recommended that you install the [Haskell language server](https://github.com/haskell/haskell-language-server) (`hsl`). This can be installed using `ghcup` or some integrated development environments, such as [Visual Studio Code](https://code.visualstudio.com/), have [excellent `hsl` integration](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).

## Defining features

At this time, `hasklepias` can be used for experimenting with `Feature` definitions. A `Feature d` is currently a synonym for an [`Either`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Either.html) type:

```haskell
type Feature d = Either MissingReason d
```

The `Either` type means there are two possibilities for the type of a `Feature`. The `Left` can be a `MissingReason`, which is a sum type enumerating the reasons that the data is missing:

```haskell
data MissingReason = -- this list may grow/change in the future
    InsufficientData
  | Excluded
  | Other String
  | Unknown
```

The `Right` has the type `d`, meaning it can be any type you choose. In the module`ExampleFeatures1`, the `index` feature has type `Feature (Interval a)`. The (`Right`) type of `index` is an `Interval a`, where again `a` can be any type you chose, subject to the constraints of intervals. The `hasDuckHistory` feature has the type `Feature (Bool, Maybe (Interval a)`, where the `Bool` is used an indicator of a history with ducks and the `Maybe (Interval a)` is the `Interval a` of the last encounter with a duck if it exists. The `countOfHospitalEvents` feature has the type `Feature (Int, Maybe b)` where the `Int` is the count of hospital visits and `Maybe b` is the duration of the last visit if one exists. These examples show how the data (or shape) of a `Feature` can be defined as `Interval a`, `(Bool, Maybe (Interval a))`, or `(Int, Maybe b)`. In fact, as long as the data is derivable from other `Feature`s and/or a list of `Event`s, you can shape a `Feature` however you'd like!

## Interactive use/development

To run the examples interactively, open a `ghci` session with:

```sh
cabal repl hasklepias:examples --repl-options -itest
```

The option flag `--repl-options -itest` allows to make changes to the files in the `examples` folder and reload with `:reload` (or `:r`) without exiting the `ghci` session. Developers working on `src` files can add the `--repl-options -isrc` option flag to make changes to `src` files too.

In `ghci` you have access to all exposed functions in `hasklepias`, `interval-algebra`, and those in the `examples` folders. For example, `exampleEvents1` is a list of events used to check some of the example features, which you can interact with:

```sh
*Main> headMay exampleEvents1
Just {(1, 10), Context {getConcepts = ["enrollment"], getFacts = Nothing, getSource = Nothing}}
*Main> length exampleEvents1
24
*Main> combineIntervals $ intervals exampleEvents1
[(1, 10),(11, 20),(21, 30),(31, 40),(45, 100)]
*Main> mapM_ print exampleEvents1
{(1, 10), Context {getConcepts = fromList ["enrollment"], getFacts = Nothing, getSource = Nothing}}
{(2, 3), Context {getConcepts = fromList ["wasScratchedByCat"], getFacts = Nothing, getSource = Nothing}}
{(5, 6), Context {getConcepts = fromList ["hadMinorSurgery"], getFacts = Nothing, getSource = Nothing}}
{(5, 10), Context {getConcepts = fromList ["tookAntibiotics"], getFacts = Nothing, getSource = Nothing}}
{(11, 20), Context {getConcepts = fromList ["enrollment"], getFacts = Nothing, getSource = Nothing}}
{(21, 30), Context {getConcepts = fromList ["enrollment"], getFacts = Nothing, getSource = Nothing}}
{(31, 40), Context {getConcepts = fromList ["enrollment"], getFacts = Nothing, getSource = Nothing}}
{(45, 46), Context {getConcepts = fromList ["wasStruckByDuck"], getFacts = Nothing, getSource = Nothing}}
<<<result truncated>>>
```
