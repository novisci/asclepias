{-|
Description : Demostrates how to define features using Hasklepias
-}

module FeatureExamples.Example2
  ( example
  ) where

import           ExampleEvents
import           Hasklepias

{-
tag::description[]

== Example 2

This example demonstrates:

* [x] the `+formMeetingSequence+` function
* [x] handling an failure case
* [x] writing a function generic over the concept type
* [x] writing a function generic over the interval type

In this example,
the goal is to write a function that,
given a list of concepts,
converts a list of events into
a list of interval durations such that:

* the events with any of the given concepts are 
combined into a "meeting sequence";
* durations of events of the resulting sequence which have 
all of the given concepts are returned;
* but an empty result is treated as a failure.

end::description[]
-}

{- tag::code[] -}
durationsOf
  :: (Eventable d c a, IntervalSizeable a b)
  => [c]
  -> [Event d c a]
  -> FeatureData [b]
durationsOf cpts =
  filter (`hasAnyConcepts` cpts) -- <1>
    .> fmap (first getConcepts . getEvent) -- <2> <3>
    .> formMeetingSequence -- <4>
    .> filter (\z -> hasAllConcepts (getPairData z) cpts) -- <5>
    .> \x -> if null x -- <6>
         then featureDataL $ Other "no cases"
         else pure (durations x)
{- end::code[] -}

{- 
tag::code-explanation[]

[TIP]
A function like this could be useful
if you wanted to find the durations of time
when a subject was both hospitalized and on some medication.

Take the case that a subject has the following events,
and we want to know the duration that a subject was 
both hospitalized and on antibiotics. 
Below, we walk through the function step-by-by using this case.

[source]
----
   --                          <- [Non-medication]
      ----                     <- [Hospitalized]
       --                      <- [Antibiotics]
            ----               <- [Antibiotics]
------------------------------
----


<1> Filter events to those that contain at least one
of the given concepts.
+
[source]
----
      ----                     <- [Hospitalized]
       --                      <- [Antibiotics]
            ----               <- [Antibiotics]
------------------------------
----

<2> Create a `+PairedInterval+` where the data is the concepts.
The `+getEvent+` function unwraps the `Event d c a` type to
a `+PairedInterval (Context d c) a+`.
The `+first+` function demonstrates the `+Bifunctor+` instance
of `+PairedInterval+`, 
applying `+getConcepts+` to tranform the type
`+PairedInterval (Context d c) a+` to
`+PairedInterval (Concepts c) a+`. 
<3> This step is important for the `+formMeetingSequence+` function,
as it requires the "data" part of the paired interval to be a `+Monoid+`.
`+Concepts+` are a `+Monoid+` by unioning the elements of two values.
<4> Form a sequence of intervals where one meets the next. 
The data of the running example would look like:
+
[source]
----
      -                        <- [Hospitalized]
       --                      <- [Hospitalized, Antibiotics]
         -                     <- [Hospitalized]
          --                   <- []
            ----               <- [Antibiotics]
------------------------------
----

<5> Filter to those intervals that have both of the given concepts. 
+
[source]
----
       --                      <- [Hospitalized, Antibiotics]
------------------------------
----

<6> Lastly, if the result of the previous step is empty,
we return a failure, i.e. a `+Left+` value of `FeatureData`.
Otherwise, we return the durations of any intervals,
as a successful `+Right+` value of `+FeatureData+`.

end::code-explanation[]
-}

example :: TestTree
example = testGroup
  "this group"
  [ testCase ""
  $   durationsOf (["tookAntibiotics", "wasHospitalized"] :: [Text])
                  exampleEvents1
  @?= featureDataL (Other "no cases")
  , testCase "TestName"
  $   durationsOf (["tookAntibiotics", "wasHospitalized"] :: [Text])
                  exampleEvents3
  @?= pure [3, 2]
  ]


