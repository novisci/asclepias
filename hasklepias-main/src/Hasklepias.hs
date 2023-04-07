{-|
Module      : Hasklepias
Description : Everything you should need to get up and running with
              hasklepias.
Copyright   : (c) Target RWE 2023
License     : BSD3
Maintainer  : bbrown@targetrwe.com
              ljackman@targetrwe.com dpritchard@targetrwe.com
-}

module Hasklepias
  ( -- * Overview
    -- $overview

    -- ** Terminology
    -- $terminology

    -- ** Cohort construction
    -- $cohort-construction

    -- *** Using 'Variable'
    -- $using-variable
    module Variable,

    -- *** Using the CLI
    -- $using-cli-main
    -- *** AWS credentials
    -- $using-cli-main

    -- *** 'cohortMain' guarantees
    -- $cohortmain-guarantees

    -- *** Input data format
    -- $using-cli-input

    -- *** Output data format
    -- $using-cli-output
    --
    -- *** Logs and runtime errors
    -- $logs-errors

    -- * Event Data
    {-|
    Events depend heavily on the [interval-algebra library](https://hackage.haskell.org/package/interval-algebra).
    See that pacakge's documentation for information about the types and functions
    for working with intervals.
    -}
    module EventDataTheory

    -- * Working with Features
    -- $features
  , module Features

    -- * Intervals based on an index
  , module Hasklepias.AssessmentIntervals

    -- * Specifying and building cohorts
  , module Cohort

    -- ** Create a cohort application
  , module Hasklepias.CohortApp
    -- ** Create an application for filtering subjects
  , module Hasklepias.LineFilterApp

    -- * Rexported Functions and modules
  , module Hasklepias.Reexports
    -- * Getting started
    -- $gettingstarted
  ) where



import           EventDataTheory

import           Cohort                         hiding (foldl', null)
import           Features
import           Variable

import           Hasklepias.AssessmentIntervals
import           Hasklepias.CohortApp
import           Hasklepias.LineFilterApp
import           Hasklepias.Reexports

{- $overview
`Hasklepias` provides an API for constructing a command-line program to
produce analysis-ready datasets for epidemiological studies, referred to
as 'cohorts' in this documentation. It also provides some domain-aware types
and utilities to help Haskell programmers write custom logic for cohort
construction, to feed the application.

Users provide custom logic, written in Haskell, to transform subject-level
input data into output data with computed variables and classifications of
elements into one or more cohorts.

The following diagram describes the primary workflow `Hasklepias` supports, at
a high level.

@
                                         +---------------------------------+
    +--------------------------+         |Configure cohort construction    |
    |Write cohort-building code|-------->| via CohortSpecMap type          |
    | computing subject-level  |         +---------------------------------+
    | * index times            |            |
    | * inclusion criteria     |            |
    | * output variables       |            |
    +--------------------------+            |
                                            |
                +---------------------------|--------+
                |Create an executable Cabal |        |
                | package component that    |        |
                | runs the transformation   |        |
                |                           v        |
                |  +------------------------------+  |
                |  |provide CohortSpecMap value as|  |
input data ---> |  | input to cohortMain to create|  | ---> output cohorts
                |  | the data processing          |  |
                |  | tranformation                |  |
                |  + -----------------------------+  |
                |                                    |
                +------------------------------------+
@

-}

{- $terminology
/Subject/ refers to the unit of input data, which internally is assigned a
subject id for processing. Internally, it has type @Subject t m a@.

/Subject-level data/ refers to @[Event t m a]@, a list of events associated
with a subject.

/Index time/ is a special temporal interval serving as reference for
inclusion and exclusion criteria as well as computed output variables. A
subject might have zero or more index times, as computed by a user-supplied
function.

/Cohort/ refers to the output produced by applying the functions in a single
@CohortSpec@ to a list of subjects. Internally, it has type @Cohort a@.

/Observational unit/ refers to the unit element of a given cohort. There is
one observational unit for each subject and index time. Each observational
unit has an identifier marking which subject and index time it is asssociated
with.

/Attrition information/ refers to the counts of: number of subjects processed,
number of observational units processed, and number of units in each inclusion
/ exclusion category.
-}

{- $cohort-construction
The 'CohortSpec' type is the only way a user can influence how input data are
processed into output data as /cohorts/. It is
parameterized as @'CohortSpec' t m a@, where the parameters
match those of the subject-level data inputs in a
non-empty list of @'Event' t m a@.

One @'CohortSpec' t m a@ is provided for each cohort to be
built, denoted by 'Text' labels in @Map Text (CohortSpec t
m a)@, with alias @'CohortSpecMap' t m a@. The map is passed
directly to 'cohortMain'.

Specifically, a user must construct a 'CohortSpec' with the functions shown in the type definition:

@
data CohortSpec t m a
  = MkCohortSpec
      { runIndices   :: NonEmpty (Event t m a) -> IndexSet a
      , runCriteria  :: NonEmpty (Event t m a) -> Interval a -> Criteria
      , runVariables :: NonEmpty (Event t m a) -> Interval a -> VariableRow
      }
@

Each of those functions processes input data in a
'Data.List.NonEmpty.NonEmpty' list of @'Event' t m a@ associated with a single
subject. Subject input data is processed one at a time, cohort-by-cohort.
Subjects without any data cannot be processed in a well-defined manner, and
using the nonempty list type prevents such cases.

@'runIndices'@ constructs the set of index times for a given subject, which are
given as @'Interval' a@, the same type as the underlying temporal element of an
@'Event' t m a@.

@'runCriteria'@ constructs a non-empty list of inclusion / exclusion criteria for
a single subject, relative to a particular index time.

@'runVariables'@ defines the output data of @'VariableRow'@ type, for each
subject and index time. This will produce one set of variables per input
subject per index time.
-}

{- $cohortmain-guarantees
'cohortMain' provides a controlled means to run the user-provided logic in a
'CohortSpec' to subject-level data. A programmer creating an executable with
'cohortMain' cannot inject arbitrary logic into the application.

In exchange for that lack of flexibility, the application runner 'cohortMain'
seeks to provide a set of guarantees about how a cohort will be built from the
user specification.

- Only the user-defined code in @'CohortSpec' t m a@ will ever manipulate,
perform operations on or otherwise interact with the subject-level input data
in a @'NonEmpty' ('Event' t m a)@.
- Functions in each 'CohortSpec' are run only on input event data only for a
single subject at a time. Input events from one subject do not influence
results of another.
- 'runIndices' produces exactly one @'IndexSet' a@ containing @'Interval' a@
values, the same type of value as the temporal component of the subject's
input events.
- If a subject's 'IndexSet' is empty, the subject is dropped from the cohort
data output, has no variables computed, and is counted only in the
'subjectsProcessed' field of the output attrition information. At present,
there is no means to mark or otherwise inspect the values or even the number of
subjects with no index value. That will change soon, and a subject without
index will be treated as an unexpected error.
- Each @'Interval' a@ in an @'IndexSet' a@ appears exactly once. Uniqueness is
determined by the 'begin' and 'end' of the interval.
- The user-supplied 'runCriteria' function must produce a non-empty list of
value type 'Criterion'. That is enforced at compile time in the user's code,
when a 'CohortSpec' value is created.
- Each observational unit is associated with exactly one subject and one index
time. The @'Interval' a@ index time to which an observational unit is associated
is part of the identifier for an observational unit, along with the subject
identifier.
- Each observational unit has a list of 'Criteria', as defined by
'runCriteria'.
- Observational units whose 'Criteria' contain at least one 'Exclude' status
value are dropped from the cohort data output, and 'runVariables' is not
computed. In the output attrition information, excluded units contribute +1 to
the 'unitsProcessed' element of the attrition information. The unit also
contributes +1 to the count of units excluded with a given label.
- An excluded observational unit is considered to be excluded by the first
'Criterion', in order of the list of 'Criteria'. The 'statusLabel' supplied
for that 'Criterion' determines how the unit is counted in the output
attrition information.
- Observational units whose 'Criteria' contain only 'Include' status values
are maintained in the cohort data output, and 'runVariables' is computed on
the unit's associated input data and index time. For the output attrition
information, included units contribute +1 to the 'unitsProcessed' and +1 to
the count of 'Included' units.
- Whether excluded or included in the cohort, units common to a single subject
together contribute +1 to the number of 'subjectsProcessed'.

-}

{- $using-variable
    This module defines the return type of 'Cohort.runVariables' along with the
    type system for the supported output targets.

    Currently the only target type system that is supported by asclepias is a
    subset of the R type system.
    
    The 'VariableRow' type is a list of 'Variable's. The intention is that the
    list of 'Variable's that is returned by 'Cohort.runVariables' after being
    applied to a single observational unit's 'Event's represents one row of
    data in a data frame, with one row per observational unit and one columns
    per study variable.
    
    The 'Variable' type wraps an underlying R type along with some metadata. The
    type is not designed to be convenient to work with once it has been created,
    so it is recommended to only produce `Variable` values when assembling the
    return value from 'Cohort.runVariables'. Instead, for intermediate values the
    recommended approach is to use one of the types used to represent R values,
    such as 'RTypeRep', 'Factor', or 'Stype'.
    
    All of the types used to represent R types in this module are either directly
    or indirectly based upon the 'RTypeRep' type. The R types that are supported
    by 'RTypeRep' are the atomic R vectors types as well as generic vectors (i.e.
    lists). These vectors are modeled as arrays of @Maybe (SEXPElem s)@s and
    where @Nothing@s represent @NA*@ values in the case of atomic vectors (see
    the type documentation for more detail for a @Nothing@ value for the list
    case).
    
    The elements of the 'RTypeRep' vector representations are modeled using the
    'SEXPElem' type family. The types in this type family for the atomic types
    correpond to the underlying element "type" that the various R atomic vectors are based
    upon. "Type" is in quotations because R's SEXPTYPE does not provide a
    separate type for the elements of an atomic vector: All singletons are
    single-length vectors. For example, the @SEXPElem 'INTSXP@ type is a
    synonym for @Int32@, which is the same type that R INTSXP values (i.e. R
    integer vectors) are based upon.
    
    The @SEXPElem 'VECSXP@ type is used to represent R VECSXP values (i.e. R
    generic vectors); see the type documentation for details.

    -}

{- $using-cli-main

Your project's Haskell program will include some @Main.hs@ module which should look like this

@
module Main where

import MyProj
import Hasklepias (cohortMain)

main :: IO ()
main = cohortMain specs
@

Here @specs@ is your project-specific 'CohortSpecMap', which in this case is
defined in some other module @MyProj@.

Then, you can @cabal run@ to build and execute the program.

To view the available command-line options, for a target named @myproj@, do

@
cabal run -v0 myproj -- --help
@
-}

{- $aws-credentials
If running an cohort application with the @--s3in@ or @--s3out@ flags, you will
need to provide AWS credentials from one of the follwing sources:

* environment variables @AWS_ACCESS_KEY_ID@ and @AWS_ACCESS_KEY_SECRET@ or
@AWS_SECRET_ACCESS_KEY@.
* configuration file in the format used by the @aws@ cli. See "Short-term
credentials" in the @aws@
  [documentation](https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html).
* Ec2 instance metadata.

A cohort application will check all three sources *in the order shown*, from
top to bottom, before throwing an exception if none can be found. The default
credentials file location is @$HOME/.aws/credentials@, with default profile
name @default@. Both values are configurable via a cohort application's
command-line interface.
-}

{- $using-cli-input
The input data, regardless of source, must be in the event lines format,
which is a JSON Lines format. Lines of input failing to parse into the
appropriate format will be logged. See Logs produced.
-}

{- $using-cli-output
    An application created with 'cohortMain' will produce JSON output of a
    consistent format. Only the 'VariableRow' JSON output differs in shape
    based on the variables produced in 'Cohort.runVariables'. See documentation
    for that type for details.

    The following example demonstrates the JSON shape produced. Here, "main"
    refers to a single cohort called "main". The output object can include one
    or more elements, all of which have the shape of "main" below.

@

{
  "main": {
    "attritionJSON": {
      "attritionByStatus": [
        [
          {
            "tag": "Included"
          },
          2
        ],
        [
          {
            "contents": "continuousEnrollment",
            "tag": "ExcludedBy"
          },
          1
        ]
      ],
      "subjectsProcessed": 4,
      "unitsProcessed": 3
    },
    "cohortJSON": [
      {
        "obsIdJSON": {
          "fromSubjId": "3",
          "indexTime": [
            "2015-01-02",
            "2015-01-03"
          ]
        },
        "variableRowJSON": [
          {
            "attrs": {
              "varName": "ageAtIndex",
              "varType": "INTSXP"
            },
            "subAttrs": {
              "long_label": "Age at day of index, computed from January 7 of smallest provided birth year.",
              "short_label": "Age at day of index",
              "special_attrs": [
                "51"
              ],
              "study_role": null,
              "stypeType": "v_nominal"
            },
            "vals": [
              51
            ],
            "varTarget": "StypeVector"
          },
          {
            "attrs": {
              "varName": "primaryOutcome",
              "varType": "INTSXP"
            },
            "subAttrs": [],
            "vals": [
              null
            ],
            "varTarget": "RVector"
          }
        ]
      }
    ]
  }
}
@

  - "main" provides the top-level field for data on a given cohort, in this
  case called "main".
  - "attritionJSON" is an @object@ providing a summary of attrition information
  for the cohort.
  - "cohortJSON" is an @array@ of the output data produced for the cohort. It
  can be thought of as an array of rows, with one row per observational unit of
  the cohort. Specifically, each element is an @object@ with two fields:
  "obsIdJSON" and "variableRowJSON".
  - "obsIdJSON" is an @object@ providing the subject id and index time pair
  uniquely identifying the observational unit.
  - "variableRowJSON" provides an @array@ of all output 'Variable' values for
  that observational unit. See 'VariableRow' for details on the shape of
  elements in this array.
-}


{- $logs-errors
At the moment, failure modes are not configurable. Logging is configurable via environment variables, as described in the top-level [Blammo configuration documentation](https://hackage.haskell.org/package/Blammo-1.1.1.1).

An application created with 'cohortMain' will fail at runtime if

- The command-line options parser,
[`execParser`](https://hackage.haskell.org/package/optparse-applicative-0.17.0.0/docs/Options-Applicative-Extra.html#v:execParser)
fails.
- File reads / writes fail.
- AWS and network errors, if input or output location is
'S3Input'.
- None of the input data event lines was parsed correctly, including cases in
which there were no lines of input at all.

Logs produced at the 'info' level:

- The location from which data are read, as passed to the application from
command-line options.
- The names of cohorts to be built, meaning the labels passed to 'cohortMain'
in user-defined code via keys of @Map Text (CohortSpec t m a)@.
- The request http status and S3 object [ETag hash
](https://docs.aws.amazon.com/AmazonS3/latest/API/API_Object.html), when S3 is
the input source or output destination.

Note the application squashes AWS logs themselves, as produced in the
`amazonka` package functions. These could be added if needed but are not
included as yet because they are verbose.

Logs produced at the 'error' level:

- Event line parsing errors, with a reference to the line of input that failed.
Note the application continues to process correctly parsed lines, if able.
- A message stating no subject-level data parsed correctly, when applicable.
The application then exits.
-}


{- $gettingstarted

To get started, you'll need to install the Haskell toolchain, especially the
[Glasgow Haskell Compiler](https://www.haskell.org/ghc/) (GHC) and the building
and packaging system [cabal](https://www.haskell.org/cabal), for which you can
use the [@ghcup@ utility](https://www.haskell.org/ghcup).

You can use any development environment you choose, but to be more productive
you should install the [Haskell Language
Server](https://github.com/haskell/haskell-language-server)
(@hls@). @hls@ can be installed using @ghcup@. Some integrated development
environments, such as [Visual Studio Code](https://code.visualstudio.com/, have
[excellent @hls@ integration](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).

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

* Setup your IDE. For example in Visual Studio Code, you'll want to install the
[Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
extension.

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

-}

{- $features

'Feature' present an interface for leveraging the type system to ensure the
inputs and outputs of functions you write are indeed what you intended, with
your intentions checked at compile time. At present, using 'Feature' s is
necessary for building an application with `Hasklepias` via 'cohortMain'.
However, that requirement will be relaxed, and 'Feature' s will be provided as
an experimental API for building cohorts, which could be integrated more
tightly with the rest of `Hasklepias` at some point.

A 'Feature' is a type parametrized by two types: @name@ and @d@. The type @d@ here
stands for data, which then parametrizes the 'FeatureData' type which is the
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
of a 'FeatureData', then, is either a @'Data.Either.Left' 'FeatureProblemFlag'@ or a
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
  (\ints -> if null ints then makeFeature (missingBecause $ CustomFlag "no data")
           else makeFeature $ featureDataR (length ints > 3))

feat2 :: Definition (
      Feature "hasMoreThan3" Bool
  -> Feature "someInts" [Int]
  -> Feature "sum" Int)
feat2 = define (\b ints -> if b then sum ints else 0)

ex0 = featInts []
ex0a = eval feat1 ex0 -- MkFeature (MkFeatureData (Left (CustomFlag "no data")))
ex0b = eval feat2 (ex0a, ex0) -- MkFeature (MkFeatureData (Left (CustomFlag "no data")))

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

== Further reading
'Feature' s are an example of
providing tight type-level control over a library API using phantom types, the
'name' of a 'Feature', in the same spirit as the Noonan's [Ghost of Departed
Proofs](http://kataskeue.com/gdp.pdf).
-}
