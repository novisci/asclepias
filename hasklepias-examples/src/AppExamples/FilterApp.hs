{-# LANGUAGE NoImplicitPrelude #-}

module AppExamples.FilterApp
  ( exampleFilterApp
  ) where
import           ExampleEvents
-- import           ExampleEvents                      (  ExampleModel(..) )
import           Hasklepias

-- | A basic test of the filter app: filter to subject that have at least one enrollment event.
exampleFilterApp :: FilterApp IO
exampleFilterApp = makeFilterApp
  "Prefilter test app"
  (getPredicate isEnrollmentEvent :: Event Text ExampleModel Day -> Bool)

isEnrollmentEvent :: Predicate (Event Text ExampleModel Day)
isEnrollmentEvent = Predicate
  (\x -> case getFacts (getContext x) of
    Enrollment -> True
    _          -> False
  )
