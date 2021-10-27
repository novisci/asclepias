{-# LANGUAGE NoImplicitPrelude #-}

module Hasklepias.ExampleFilterApp
  ( exampleFilterApp
  ) where
import           Hasklepias

-- | A basic test of the filter app: filter to subject that have at least one enrollment event.
exampleFilterApp :: FilterApp IO
exampleFilterApp = makeFilterApp "Prefilter test app" (getPredicate isEnrollmentEvent :: Event Day -> Bool)
