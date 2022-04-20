{-# LANGUAGE DataKinds #-}
module FeatureExamples.DrugDiscontinuation where


import           ExampleEvents
import           Hasklepias

-- | time of distcontinuation of antibiotics
--   and time from start of follow up
--   TODO This needs to be generalized as Nothing could either indicate they didn't 
--   discontinue or that they simply got no antibiotics records.


{- tag::function[] -}
discontinuation
  :: (IntervalSizeable a b)
  => AssessmentInterval a
  -> [Event Text ExampleModel a]
  -> Maybe (a, b)
discontinuation i events =
  (\x -> Just
      ( begin x       -- we want the begin of this interval 
      , diff (begin x) (begin i)
      )
    )
    =<< headMay                    -- if there are any gaps the first one is the first discontinuation
    =<< gapsWithin i               -- find gaps to intervals clipped to i
    =<< ( nothingIfNone (so (getInterval i))      -- if none of the intervals start or overlap 
                        -- the followup, then never started antibiotics
        . combineIntervals          -- combine overlapping intervals
        . map (expandr 5)           -- allow grace period
        . filterEvents (containsConcepts         -- filter to only antibiotics events
                                         ["tookAntibiotics"])
        )
          events
  where so = unionPredicates [startedBy, overlappedBy]
{- end::function[] -}

{- tag::definition[] -}
discontinuationDef
  :: (IntervalSizeable a b)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ExampleModel a]
       -> Feature "discontinuation" (Maybe (a, b))
       )
discontinuationDef = define discontinuation
{- end::definition[] -}
