{-|
Description : Demostrates how to define features using Hasklepias
-}

module FeatureExamples.Example3
  ( example
  ) where

import           ExampleEvents                  ( exampleEvents4 )
import           Hasklepias

examplePairComparison
  :: (IntervalSizeable a b) => Interval a -> [Event d Text a] -> (Bool, Maybe a)
examplePairComparison i es =
  es
    |> filterConcur i                   -- filter to concurring with followup interval    
    -- NOTE: function by the same name was in hasklepias-core FeatureData.
    -- analogous functionality but slightly different signature to accommodate
    -- new Event, e.g. Ord constraint on c in Event d c a. Note however that
    -- the concepts are Text, so we fix type c to Text and don't need to
    -- include the constraint in examplePairComparison's signature.
    |> splitByConcepts ["c1"] ["c2"]    -- form a list of pairs where first element
    |> uncurry allPairs                 -- has "c1" events and second has "c2" events    

    |> filter                           -- filter this list of pairs to cases 
              (\pr -> fst pr `concur`             -- where "c1" event concurs with +/- 3
                                      expand 3 3 (snd pr))           -- of any "c2" event 
    |> fmap fst
    |> (\x ->
         ( isNotEmpty x                  -- are there any?
         , fmap begin (lastMay x)
         )
       )      -- if exists, keep the begin of the last "c1" interval

flwup :: FeatureData (Interval Int)
flwup = pure $ beginerval 50 0

example :: TestTree
example = testGroup "examplePairComparison" 
  [ testCase "" $  liftA2 examplePairComparison flwup (pure exampleEvents4)
    @?= pure (True, Just 16) ]
    
