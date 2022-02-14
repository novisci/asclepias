{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Planning.Examples.Simple where

-- NOTE dhall imports are just for this example and not needed for a typical
-- project
import           Data.Text                      ( Text )
import           Data.Void
import qualified Dhall
import           Dhall.Core                     ( Expr(..)
                                                , RecordField(..)
                                                , Chunks(..)
                                                )
import qualified Dhall.Map
import           Dhall.Src                      ( Src )
import           IntervalAlgebra
import           Map.Internal
import           Planning.Event
import           Planning.Test

  {- Project-specific code

      This section gives a sense of how much project-speicifc code needs to be written to interact with hygiea's testing structure. In this case, since outputs and inputs are aliases for CensoredOccurence and Event, the only hygiea-specific code is the ToOutput instance.
      -}

instance ToOutput ProjInterval ProjOccurrence where
  toOutput = cohortBuilderSingle

instance ToOutput [ProjEvent] [ProjOccurrence] where
  toOutput = cohortBuilder index

-- cohort builder: just some nonsense
cohortBuilderSingle :: ProjInterval -> ProjOccurrence
cohortBuilderSingle = MkCensoredOccurrence "after"

-- TODO Note ths example doesn't use cohortBuilder. Would need to decode a List of records. 
cohortBuilder :: Index -> [ProjEvent] -> [ProjOccurrence]
cohortBuilder ix = foldr op []
 where
  e = end ix
  op (MkEvent x) xs =
    if end (getInterval x) >= e then cohortBuilderSingle x : xs else xs



-- Project-specific types

-- there is nothing to do for hygeia, since these alias the generic event, for
-- which constraints are already implemented, and Integer, Text already
-- implement the necessary conversions
type ProjEvent = Event Text Text Integer
type ProjInterval = PairedInterval (Context Text Text) Integer
type Index = Interval Integer
type ProjOccurrence = CensoredOccurrence Text (Context Text Text) Integer

index :: Index
index = beginervalMoment 0


  {- Example data for testing

      This all would get parsed from CSV and is only for demonstration in this "planning phase" example
      -}

-- maps that would be parsed from dhall, to try them directly in testIt
-- using the cohortBuilderSingle below
-- see app for results
testOutput :: TestMap
testOutput = fromList
  [ ("begin"   , TInteger 0)
  , ("end"     , TInteger 4)
  , ("reason"  , TText "after")
  , ("concepts", TText "home")
  , ("facts"   , TText "is_funny")
  ]

testInput :: TestMap
testInput = fromList
  [ ("begin"   , TInteger 0)
  , ("end"     , TInteger 4)
  , ("concepts", TText "home")
  , ("facts"   , TText "is_funny")
  ]

makeSimpleRecordField :: Expr Src Void -> RecordField Src Void
makeSimpleRecordField e = RecordField Nothing e Nothing Nothing


-- in dhall format, to check the decoder/encoder
testOutputDhall :: Expr Src Void
testOutputDhall = RecordLit $ Dhall.Map.fromList
  [ ("begin", makeSimpleRecordField (IntegerLit 0))
  , ("end"  , makeSimpleRecordField (IntegerLit 0))
  , ("reason"  , makeSimpleRecordField (TextLit (Chunks [] "after")))
  , ("concepts"  , makeSimpleRecordField (TextLit (Chunks [] "home")))
  , ("facts"  , makeSimpleRecordField (TextLit (Chunks [] "is_funny")))
  ]
testInputDhall :: Expr Src Void
testInputDhall = RecordLit $ Dhall.Map.fromList
  [ ("begin", makeSimpleRecordField (IntegerLit 0))
  , ("end"  , makeSimpleRecordField (IntegerLit 4))
  , ("concepts"  , makeSimpleRecordField (TextLit (Chunks [] "home")))
  , ("facts"  , makeSimpleRecordField (TextLit (Chunks [] "is_funny")))
  ]

-- TODO fix the naming confusion
testMapInputSchema, testMapOutputSchema :: Expr Src Void -> Text -> IO TestMap
testMapInputSchema = mapInputSchema
testMapOutputSchema = mapInputSchema

testInputText :: Text
testInputText = "{ begin = +0, end = +4, concepts = \"home\", facts = \"is_funny\" }"

testOutputText :: Text
testOutputText = "{ begin = +0, end = +4, concepts = \"home\", facts = \"is_funny\", reason = \"after\" }"
