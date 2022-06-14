module Templates.Features.BuildIsEnrolled
  ( buildIsEnrolled
  , buildIsEnrolledTests
  ) where

import           Data.Tuple.Solo
import           Templates.FeatureReqs         as F -- TODO: remove this import; it's necessary (for some reason for GHC 9.0.1)

{- tag::template0[] -}
buildIsEnrolled
  :: ( Intervallic i0 a
     , Monoid (container (Interval a))
     , Applicative container
     , Witherable container
     )
  => Predicate (Event c m a) -- ^ The predicate to filter to Enrollment events (e.g. 'FeatureEvents.isEnrollment')
  -> Definition
       (  Feature indexName (i0 a)
       -> Feature eventsName (container (Event c m a))
       -> Feature varName Status
       )
buildIsEnrolled predicate = define
  (\index ->
    F.filterEvents predicate
      .> combineIntervals
      .> any (concur index)
      .> includeIf
  )
{- end::template0[] -}

type IsEnrolledArgs = (Solo (Predicate (Event Text TestSchema Int)))
  -- use of Solo is because buildIsEnrolled takes a single argument and the
  -- Solo is needed to match the Curry constraints in the makeBuilderAssertion
  -- (via makeTestGroup). In the buildIsEnrolledTestCases, the Solo is created 
  -- by its Applicative instance using `pure`. 

type IsEnrolledTestCase
  = TestCase
      (F "index" (Interval Int), F "events" [Event Text TestSchema Int])
      Status
      IsEnrolledArgs

buildIsEnrolledTestCases :: [IsEnrolledTestCase]
buildIsEnrolledTestCases =
  [ f "Exclude if no events" (pure isEnrollmentEvent) (0, 1) [] Exclude
  , f "Exclude if only interval meets"
      (pure isEnrollmentEvent)
      (0, 1)
      [g (1, 6)]
      Exclude
  , f "Include if concurring interval"
      (pure isEnrollmentEvent)
      (0, 1)
      [g (-1, 4)]
      Include
  , f "Include if concurring interval"
      (pure isEnrollmentEvent)
      (0, 1)
      [g (-1, 1), g (1, 4)]
      Include
  ] where
  f = makeTestCaseOfIndexAndEvents
  g = makeEnrollmentEvent


buildIsEnrolledTests :: TestTree
buildIsEnrolledTests = makeTestGroup "Tests of isEnrolled template"
                                     buildIsEnrolled
                                     buildIsEnrolledTestCases
