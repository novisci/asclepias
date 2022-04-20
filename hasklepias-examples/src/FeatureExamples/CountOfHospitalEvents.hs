{-# LANGUAGE DataKinds #-}
module FeatureExamples.CountOfHospitalEvents where
import           ExampleEvents
import           Hasklepias



-- | Count of hospital events in a interval and duration of the last one
{- tag::function[] -}
countOfHospitalEvents
  :: (IntervalSizeable a b)
  => AssessmentInterval a
  -> [Event Text ExampleModel a]
  -> (Int, Maybe b)
countOfHospitalEvents i =
  (\x -> (length x, duration <$> lastMay x))
    . filterNotDisjoint i                    -- filter to intervals not disjoint from interval
    . combineIntervals                       -- combine overlapping intervals
    . filterEvents (containsConcepts ["wasHospitalized"]) -- filter to only antibiotics events
{- end::function[] -}

{- tag::definition[] -}
countOfHospitalEventsDef
  :: (IntervalSizeable a b)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ExampleModel a]
       -> Feature "count of hospitalizations" (Int, Maybe b)
       )
countOfHospitalEventsDef = define countOfHospitalEvents
{- end::definition[] -}
