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

In this example,
the goal is to write a function that
converts a list of events into a list of interval durations where

* foo
* bar

.Example2
image::sunset.jpeg[]

This example demonstrates:

* [x] lots of cool stuff
* [x] even more cool stuff
* [x] writing a function generic over the interval type

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
    .> fmap (first getConcepts . getEvent) -- <2>
    .> formMeetingSequence
    .> filter (\z -> hasAllConcepts (getPairData z) cpts)
    .> \x -> if null x
         then featureDataL $ Other "no cases"
         else pure (durations x)
{- end::code[] -}

{- 
tag::code-explanation[]
<1> filter events
<2> create a paired interval where the data is the concepts
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


