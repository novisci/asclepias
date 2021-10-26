{-# LANGUAGE NoImplicitPrelude #-}

module Hasklepias.ExampleFilterApp
  ( exampleFilterApp
  ) where
import           Hasklepias

exampleFilterApp :: FilterApp IO
exampleFilterApp = makeFilterApp "test" (getPredicate isEnrollmentEvent :: Event Day -> Bool)
