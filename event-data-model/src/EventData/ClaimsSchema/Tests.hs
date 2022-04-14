{-|
Module      : Testing the event data model
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module EventData.ClaimsSchema.Tests
  ( claimsSchemaTests
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           EventData.ClaimsSchema
import           EventData.ClaimsSchema.Accessors
import           EventData.ClaimsSchema.Predicates
import           EventDataTheory
import           EventDataTheory.Test
import           Test.Tasty
import           Test.Tasty.HUnit


-- | Toy events for unit tests
e1 :: Event Text ClaimsSchema Int
e1 = event
  (beginerval 4 1)
  (context (packConcepts ["c1", "c2"])
           (Enrollment (EnrollmentFacts { plan = Nothing }))
           Nothing
  )


e2 :: Event Text ClaimsSchema Int
e2 = event
  (beginerval 4 2)
  (context (packConcepts ["c3", "c4"])
           (Enrollment (EnrollmentFacts { plan = Nothing }))
           Nothing
  )

e3 :: Event Text ClaimsSchema Int
e3 = event
  (beginerval 4 2)
  (context
    (packConcepts [])
    (Demographics (DemographicsFacts (DemographicsInfo Gender (Just "F"))))
    Nothing
  )

demoYear :: ClaimsSchema
demoYear =
  Demographics $ DemographicsFacts (DemographicsInfo BirthYear (Just "1987"))

e4 :: Event Text ClaimsSchema Int
e4 = event (beginerval 4 2) (context (packConcepts []) demoYear Nothing)

enrollEvent :: B.ByteString
enrollEvent =
  "[\"abc\",0,1,\"Enrollment\",[]\
  \,{\"patient_id\":\"abc\"\
  \,\"time\":{\"begin\":0,\"end\":1}\
  \,\"domain\":\"Enrollment\"\
  \,\"facts\":{\"plan\":{\"benefit\":\"PPO\",\"exchange\":\"None\"}}}]"

-- | Unit tests on predicate functions
predicateUnitTests :: TestTree
predicateUnitTests = testGroup
  "Unit tests on ClaimsSchema predicate functions"
  [ testCase "isBirthYearEvent on demographic domain with BirthYear"
    $   getPredicate isBirthYearEvent e4
    @?= True
  ]

-- | Unit tests on accessor functions
accessorUnitTests :: TestTree
accessorUnitTests = testGroup
  "Unit tests on ClaimsSchema accessor functions"
  [ testCase "viewGenders on empty list" $ viewGenders [] @?= []
  , testCase "viewGenders with no demo events" $ viewGenders [e1, e2] @?= []
  , testCase "viewGenders with a demo event" $ viewGenders [e1, e3] @?= ["F"]
  , testCase "previewBirthYear on demographic domain"
  $   previewBirthYear demoYear
  @?= Just 1987
  , testCase "viewBirthYears on demographic event"
  $   viewBirthYears [e3, e4]
  @?= [1987]
  , testCase "viewBenefits on enrollment event"
  $   fmap
        viewBenefits
        (sequenceA
          [ snd
              <$> decodeEvent @ClaimsSchema @Text @Int
                    defaultParseEventLineOption
                    enrollEvent
          ]
        )
  @?= Just ["PPO"]
  ]

-- | Check that files in test/claimevents-good successfully parse
decodeClaimEvents :: IO TestTree
decodeClaimEvents =
  eventDecodeTests @ClaimsSchema @Text @Day "test/claimevents-good"

claimsSchemaTests :: IO TestTree
claimsSchemaTests = testGroup "ClaimsSchema tests" <$> sequenceA
  [pure accessorUnitTests, pure predicateUnitTests, decodeClaimEvents]