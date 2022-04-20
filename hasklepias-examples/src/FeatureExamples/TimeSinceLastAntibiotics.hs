{-# LANGUAGE DataKinds #-}
module FeatureExamples.TimeSinceLastAntibiotics where

import           ExampleEvents
import           Hasklepias

-- | Time from end of baseline to end of most recent Antibiotics
--   with 5 day grace period
{- tag::function[] -}
timeSinceLastAntibiotics
  :: (IntervalSizeable a b)
  => AssessmentInterval a
  -> [Event Text ExampleModel a]
  -> Maybe b
timeSinceLastAntibiotics i =
  lastMay                                 -- want the last one
    . map (max 0 . diff (end i) . end)        -- distances between end of baseline and antibiotic intervals
    . filterNotDisjoint i                     -- filter to intervals not disjoint from baseline interval
    . combineIntervals                        -- combine overlapping intervals
    . map (expandr 5)                         -- allow grace period
    . filterEvents (containsConcepts ["tookAntibiotics"]) -- filter to only antibiotics events 
{- end::function[] -}

{- tag::definition[] -}
timeSinceLastAntibioticsDef
  :: (IntervalSizeable a b)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ExampleModel a]
       -> Feature "time since antibiotics" (Maybe b)
       )
timeSinceLastAntibioticsDef = define timeSinceLastAntibiotics
{- tag::definition[] -}
