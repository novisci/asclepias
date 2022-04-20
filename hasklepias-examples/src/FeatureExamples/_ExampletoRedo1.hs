{-|
Description : Demostrates how to define features using Hasklepias
-}

{-# LANGUAGE DataKinds #-}

module FeatureExamples.Example1
  -- ( example
  -- ) 
                                 where

import           ExampleEvents
import           Hasklepias


-- {-
--   Tests
-- -}

-- type MyData
--   = ( Feature "index" (Interval Int)
--     , Feature "enrolled" Status
--     , Feature "duck history" (Bool, Maybe (Interval Int))
--     , Feature "macaw history" (Bool, Maybe (Interval Int))
--     , Feature "two major or one minor" Bool
--     , Feature "time since antibiotics" (Maybe Int)
--     , Feature "count of hospitalizations" (Int, Maybe Int)
--     , Feature "discontinuation" (Maybe (Int, Int))
--     )

-- getUnitFeatures :: Interval Int -> [Event Text ExampleModel Int] -> MyData
-- getUnitFeatures index x =
--   ( idx
--   , eval
--     (buildContinuousEnrollment bline (containsConcepts ["enrollment"]) 60)
--     idx
--     evs
--     (pure Include)
--   , eval duckHxDef                   bl evs
--   , eval macawHxDef                  bl evs
--   , eval twoMinorOrOneMajorDef       bl evs
--   , eval timeSinceLastAntibioticsDef bl evs
--   , eval countOfHospitalEventsDef    bl evs
--   , eval discontinuationDef          fl evs
--   )
--  where
--   evs = pure x
--   idx = pure index
--   bl  = fmap bline idx
--   fl  = fmap flwup idx

-- -- just a dummy set for now
-- dummyIndex :: Interval Int
-- dummyIndex = beginerval 1 0

-- includeAll :: Interval Int -> [Event Text ExampleModel Int] -> Criteria
-- includeAll _ _ = criteria $ pure
--   (into @Criterion
--     (makeFeature (featureDataR Include) :: Feature "includeAll" Status)
--   )


-- example1results :: MyData
-- example1results =
--   ( pure (beginerval 1 (60 :: Int))
--   , pure Include
--   , pure (True, Just $ beginerval 1 (45 :: Int))
--   , pure (False, Nothing)
--   , pure True
--   , pure $ Just 4
--   , pure (1, Just 8)
--   , pure $ Just (78, 18)
--   )

-- exampleEvalOpts :: CohortEvalOptions
-- exampleEvalOpts = defaultCohortEvalOptions

-- exampleCohortSpec
--   :: CohortSpec [Event Text ExampleModel Int] MyData (Interval Int)
-- exampleCohortSpec = specifyCohort defineIndexSet includeAll getUnitFeatures

-- -- NOTE makeCohortEvaluator requires a monad wrapper in return type. Using
-- -- Either here because IO a has no Eq instance, hence testing doesn't work.
-- exampleCohortEvaluator
--   :: Population [Event Text ExampleModel Int]
--   -> Either Text (Cohort MyData (Interval Int))
-- exampleCohortEvaluator = makeCohortEvaluator exampleEvalOpts exampleCohortSpec

-- -- NOTE constructor unexported.  only way to do construct it is with `from`. 
-- examplePopulation :: Population [Event Text ExampleModel Int]
-- examplePopulation = from [exampleSubject1, exampleSubject2]

-- example :: TestTree
-- example = testGroup
--   ""
--   [ testCase "getUnitFeatures from exampleEvents1"
--   $   getUnitFeatures (beginerval 1 60) exampleEvents1
--   @?= example1results
--   , testCase "mapping population to cohort"
--   $   exampleCohortEvaluator examplePopulation
--   @?= Right
--         (MkCohort
--           ( makeTestAttritionInfo
--             2
--             1
--             [ (SubjectHasNoIndex           , 1)
--             , (ExcludedBy (1, "includeAll"), 0)
--             , (Included                    , 1)
--             ]
--           , from @[ObsUnit MyData (Interval Int)]
--             [ from @(ObsID (Interval Int), MyData)
--                 (makeObsID (beginervalMoment 60) ("a" :: Text), example1results)
--             ]
--           )
--         )
--   ]
