{-|
Module      : Hasklepias
Description : Everything you should need to get up and running with 
              hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Hasklepias
  ( -- $about

    -- * Event Data
    {-|
    Events depend heavily on the [interval-algebra library](https://hackage.haskell.org/package/interval-algebra). 
    See that pacakge's documentation for information about the types and functions 
    for working with intervals.
    -}
    module EventData
  , module EventDataTheory

    -- * Working with Features
    -- $features
  , module Features

    -- * Feature definition builders
    {- | 
    A collection of pre-defined functions which build common feature definitions
    used in epidemiologic cohorts.
    -}
  , module Templates.Features
    -- * Utilities for defining Features from Events
    {- |
    Much of logic needed to define features from events depends on the 
    [interval-algebra](https://hackage.haskell.org/package/interval-algebra) library.
    Its main functions and types are re-exported in Hasklepias, but the documentation
    can be found on [hackage](https://hackage.haskell.org/package/interval-algebra).

    -}
  -- , module Hasklepias.FeatureEvents
  , module Hasklepias.Misc

    -- * Intervals based on an index
  , module AssessmentIntervals

    -- * Specifying and building cohorts
  , module Cohort

    -- ** Create a cohort application
  , module Hasklepias.MakeCohortApp
  , module Hasklepias.AppUtilities
    -- ** Create an application for filtering subjects
  , module Hasklepias.MakeFilterApp

    -- * Statistical Types
  , module Stype

    -- * Rexported Functions and modules
  , module Hasklepias.Reexports
  , module Hasklepias.ReexportsUnsafe
  ) where

import           AssessmentIntervals

import           EventData
import           EventDataTheory

import           Features

import           Cohort

import           Hasklepias.AppUtilities
import           Hasklepias.MakeCohortApp
import           Hasklepias.MakeFilterApp
import           Hasklepias.Misc
import           Hasklepias.Reexports
import           Hasklepias.ReexportsUnsafe
import           Templates.Features

import           Stype

{- $about

@Hasklepias@ is an embedded domain specific language (eDSL) written in [Haskell](https://www.haskell.org/).
To get started, then, you'll need to install the Haskell toolchain, especially 
the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) (GHC) and the building
and packaging system [cabal](https://www.haskell.org/cabal), for which you can
use the [@ghcup@ utility](https://www.haskell.org/ghcup).

You can use any development environment you choose, but for maximum coding pleasure,
you should install the [Haskell language server](https://github.com/haskell/haskell-language-server)
(@hsl@). This can be installed using @ghcup@. Some integrated development
environments, such as [Visual Studio Code](https://code.visualstudio.com/, have 
[excellent @hsl@ integration](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).

In summary,

* Install [@ghcup@](https://www.haskell.org/ghcup).

@ 
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
@
* Inspect your toolchain installation using @ghcup list@. You will need @ghc@ 
(>= 8.10.4) , @hls@ (>= 1.2), and @cabal@ (>= 3.4) installed.
* Upgrade toolchain components as necesarry. For example:

@
  ghcup install ghc {ghcVersion}
  ghcup set ghc {ghcVersion}
  ghcup install cabal {cabalVersion}
  ghcup set cabal {cabalVersion} 
@

* Setup your IDE. (e.g. in Visual Studio, you'll want to install the [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) extension.

=== Getting started in Haskell

Since @Hasklepias@ is written in [Haskell](https://www.haskell.org/), you'll need 
to understand the syntax of Haskell function and a few concepts. The Haskell 
language is over 30 years old and has many, many features. Here are a few resources:

* [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters): good intro text
* [Programming in Haskell](https://www.cs.nott.ac.uk/~pszgmh/pih.html): excellent intro text
* [What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/): excellent resource
* [Haskeller competency matrix](https://gist.github.com/graninas/833a9ff306338aefec7e543100c16ea1)
* [Hoogle](https://hoogle.haskell.org/): search engine for Haskell functions
* [5 years of Haskell in production](https://www.youtube.com/watch?v=hZgW4mT1PkE[): video on using Haskell in production environment
* [Things software engineers trip up on when learning Haskell](https://williamyaoh.com/posts/2020-04-12-software-engineer-hangups.html): a software engineer's list of tips on using Haskell

=== Interacting with the examples (using GHCi)

To run the examples interactively, open a @ghci@ session with:

@
cabal repl hasklepias:examples 
@

In @ghci@ you have access to all exposed functions in @hasklepias@, @interval-algebra@, 
and those in the [examples](https://github.com/novisci/asclepias/tree/master/examples) folder.

-}

{- $features

A 'Feature' is a type parametrized by two types: @name@ and @d@. The type @d@ here
stands for "data", which then parametrizes the 'FeatureData' type which is the 
singular value which a 'Feature' contains. The @d@ here can be almost anything
and need not be a scalar, for example, all the following are valid types for @d@:

* 'Int'
* 'Text'
* @(Int, Maybe Text)@ 
* @[Double]@

The @name@ type a bit special: it does not appear on the right-hand side of the `=`. 
In type-theory parlance, @name@ is a [phantom type](https://wiki.haskell.org/Phantom_type). 
We'll see in a bit how this can be useful. For now, think of the @name@ as the
name of a variable as you would in most programming languages. To summarize, 
a `Feature` 's type constructor takes two arguments (@name@ and @d@), but its
*value* constructor (@MkFeature@) takes a single value of type @FeatureData d@.

Values of the 'FeatureData' type contain the data we're ultimately interested 
in analyzing or passing along to downstream applications. However, a 'FeatureData'
value does not simply contain data of type @d@. The type allows for the possibility
of missingness, failures, or errors by using the 'Data.Either.Either' type. A value
of a 'FeatureData', then, is either a @'Data.Either.Left' 'MissingReason'@ or a
@'Data.Either.Right' d@.

The use of @Either@ has important implications when defining Features, as we will see. 
Now that we know the internals of a 'Feature', how do we create 'Feature' s? There
are two ways to create features: (1) 'pure'ly lifting data into a 'Feature' or
(2) writing a 'Definition': a function that 'define's a 'Feature' based on other 
'Feature's. 

The first method is a way to get data directly into a 'Feature'. Fhe following
function takes a list of 'Events' and makes a 'Feature' of them:

@
allEvents :: [Event Day] -> Feature "allEvents" [Event Day]
allEvents = pure
@

The 'pure' lifting is generally used to lift a subject's input data into a 'Feature',
so that other features can be defined from a subject's data. 'Feature' s are
derived from other 'Feature's by the 'Definition' type. Specifically, 
'Definition' is a type which contains a function which maps 'Feature' inputs
to a 'Feature' output, for example:

@
myDef :: Definition (Feature "a" Int -> Feature "b" Bool)
myDef = define (\x -> if x > 0 then True else False)
@

A 'Definition' is created by the 'define' (or 'defineA') function. One may ask
why 'define' is necessary, and we don't directly define the function 
(@Feature "a" Int -> Feature "b" Bool@) directly. What may not be obvious in 
the above, is that @x@ is type @Int@ not @Feature "a" Int@ and the return type 
is @Bool@ not @Feature "b" Bool@. The 'define' function and 'Definition' type
do the magic of lifting these types to the 'Feature' level. To see this, 
in the following, @myDef2@ is equivalent to @myDef@: 

@
intToBool :: Int -> Bool
intToBool x = if x > 0 then True else False)

myDef2 :: Definition (Feature "a" Int -> Feature "b" Bool)
myDef2 = define intToBoo
@

The 'define' function, then, let's us focus on the *logic* of our 'Feature's 
without needing to worry handling the error cases. If we were to write a function
with signature @Feature "a" Int -> Feature "b" Bool@ directly, it would look
something like:

@
myFeat :: Feature "a" Int -> Feature "b" Bool
myFeat (MkFeature (MkFeatureData (Left r))) = MkFeature (MkFeatureData (Left r))
myFeat (MkFeature (MkFeatureData (Right x))) = MkFeature (MkFeatureData (Right $ intToBool x))
@

One would need to pattern match all the possible types of inputs, which gets
more complicated as the number of inputs increases. As an aside, since @Feature@s are
[Functors]( https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Functor.html),
one could instead write:

@
myFeat :: Feature "a" Int -> Feature "b" Bool
myFeat = fmap intToBool
@

This would require understanding how Functors and similar structures are used.
The 'define' and 'defineA' functions provide a common interface to these structures
without needing to understand the details.

== Evaluating Definitions

To evaluate a 'Definition', we use the 'eval' function. Consider the following example.
The input data is a list of 'Int's if the list is empty ('null'), this is considered
an error in @feat1@. If the list has more than 3 elements, then in @feat2@, 
the sum is computed; otherwise @0@ is returned.

@
featInts :: [Int] -> Feature "someInts" [Int]
featInts = pure

feat1 :: Definition (Feature "someInts" [Int] -> Feature "hasMoreThan3" Bool)
feat1 = defineA
  (\ints -> if null ints then makeFeature (missingBecause $ Other "no data")
           else makeFeature $ featureDataR (length ints > 3))

feat2 :: Definition (
      Feature "hasMoreThan3" Bool
  -> Feature "someInts" [Int]
  -> Feature "sum" Int)
feat2 = define (\b ints -> if b then sum ints else 0)

ex0 = featInts []
ex0a = eval feat1 ex0 -- MkFeature (MkFeatureData (Left (Other "no data")))
ex0b = eval feat2 (ex0a, ex0) -- MkFeature (MkFeatureData (Left (Other "no data")))

ex1 = featInts [3, 8]
ex1a = eval feat1 ex1 -- MkFeature (MkFeatureData (Right False))
ex1b = eval feat2 (ex1a, ex1) -- MkFeature (MkFeatureData (Right 0))

ex2 = featInts [1..4]
ex2a = eval feat1 ex2 -- MkFeature (MkFeatureData (Right True))
ex2b = eval feat2 (ex2a, ex2) -- MkFeature (MkFeatureData (Right 10))
@

Note the value of @ex0b@. It is a 'Left' because the value of @ex0a@ is a 'Left';
in other words, errors propogate along 'Feature's. If a given @Feature@'s dependency 
is a 'Left' then that 'Feature' will also be 'Left'. A @Feature@'s internal
'Either' structure has important implications for designing 'Feature's and
performance. Capturing an error in a 'Left' is a way to prevent downstream
dependencies from needing to be computed.

== Type Safety of Features

In describing the 'Feature' type, the utility of having the name as a type may
not have been clear. To clarify, consider the following example:

@
x :: Feature "someInt" Natural
x = pure 39

y :: Feature "age" Natural
y = pure 43

f :: Definition (Feature "age" Natural -> Feature "isOld" Bool)
f = define (>= 39)

fail = eval f x 
pass = eval f y
@   

In the example, @fail@ does not compile because @"someInt"@ is not @"age"@, 
even though both the data type are @Natural@.

-}
