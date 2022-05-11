{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Examples.Simple where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Bifunctor                 ( first )
-- NOTE dhall imports are just for this example and not needed for a typical
-- project
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Dhall                          ( FromDhall
                                                , ToDhall
                                                )
import           EventDataTheory
import           GHC.Generics
import           IntervalAlgebra
import           System.FilePath
import           Test.Hygiea.TestMap
import           Test.Hygiea.ToOutput
import           Test.Tasty.Hygiea
import           Witch.From
import           Witch.TryFrom

  {- Project-specific code

      This section gives a sense of how much project-specific code needs to be written to interact with hygiea's testing structure. In this case, since outputs and inputs are aliases for CensoredOccurence and Event, the only hygiea-specific code is the ToOutput instance.

      See Main.hs for how it might be used in a test.
      -}


  {- Code required for Hygiea testing -}

-- defining the conversion
instance ToOutput [ProjEvent] [ProjOccurrence] where
  toOutput = cohortBuilder index

-- specifying the files
projPath :: String
projPath = "hygiea/src/Examples"

inputCsv, outputCsv :: String
inputCsv = projPath </> "input.csv"
outputCsv = replaceFileName inputCsv "output.csv"

inputDhall, outputDhall :: String
inputDhall = projPath </> "input.dhall"
outputDhall = replaceFileName inputCsv "output.dhall"

myRoutine :: Routine
myRoutine = Golden (MkRoutineElem @[ProjEvent] inputCsv inputDhall)
                   (MkRoutineElem @[ProjOccurrence] outputCsv outputDhall)

badInputCsv, badOutputCsv :: String
badInputCsv = replaceFileName outputCsv "input_bad.csv"
badOutputCsv = replaceFileName outputCsv "output_bad.csv"

myBadRoutine :: Routine
myBadRoutine = Golden
  (MkRoutineElem @[ProjEvent] inputCsv inputDhall)
  (MkRoutineElem @[ProjOccurrence] badOutputCsv outputDhall)

myMisspecRoutine :: Routine
myMisspecRoutine = Golden
  (MkRoutineElem @[ProjEvent] badInputCsv inputDhall)
  (MkRoutineElem @[ProjOccurrence] outputCsv outputDhall)

  {- Other project code -}

-- Cohort builder
-- Change Facts field
cohortBuilderSingle :: Index -> ProjEvent -> ProjOccurrence
cohortBuilderSingle index e = event i (whatIsIt c)
  where i = getInterval $ getEvent e
        c = getContext e
        whatIsIt c'
          | end i >= end index = c{getFacts = WasAfter}
          | otherwise = c{getFacts = WasBefore}

wasAfter :: Event a SumminElse b -> Bool
wasAfter = (== WasAfter) . getFacts . getContext

-- placeholder for some filterMap operation building outcomes
-- make a ProjOccurrence with reason "after", keeping other data, if the index
-- end is <= the event end. index is (0, 1)
cohortBuilder :: Index -> [ProjEvent] -> [ProjOccurrence]
cohortBuilder ix = foldr op []
 where
  op x xs = 
    let x' = cohortBuilderSingle ix x
     in if wasAfter x' then x' : xs else xs

-- Project-specific types

-- Inputs
-- there is nothing to do for hygeia to implement the TryFrom instances, since
-- these alias the generic event, for which constraints are already
-- implemented, and Integer, Text already implement the necessary conversions
data TrueFacts = Awesome | NotAwesome Text deriving (Show, Eq, Generic)
type ProjEvent = Event Text TrueFacts Integer
type ProjInterval = PairedInterval (Context Text TrueFacts) Integer
type Index = Interval Integer

index :: Index
index = beginervalMoment 0

-- bootstrap conversion via dhall
instance FromDhall TrueFacts
instance ToDhall TrueFacts
-- json instances required for Golden
instance ToJSON TrueFacts
instance FromJSON TrueFacts

-- Outputs
data SumminElse = WasBefore | WasAfter deriving (Show, Eq, Generic)
type ProjOccurrence = Event Text SumminElse Integer

instance FromDhall SumminElse
instance ToDhall SumminElse
-- json instances required for Golden
instance ToJSON SumminElse
instance FromJSON SumminElse


-- Some data to play with in the repl

f1, f2 :: TrueFacts
f1 = Awesome
f2 = NotAwesome "ugh"

e1, e2 :: ProjEvent
e1 = event (beginerval 0 1) (context (packConcepts ["yay"]) f1 Nothing)
e2 = event (beginerval 0 1) (context (packConcepts ["not yay"]) f1 Nothing)

-- note these conversions are never ones the programmer need to do, and the
-- exception handling uses HygieaException not Text

-- simple type conversion
v1, v2 :: Either Text TestVal
v1 = case tryFrom f1 of
  Right v   -> pure v
  Left  err -> Left $ pack $ show err
v2 = case tryFrom f2 of
  Right v   -> pure v
  Left  err -> Left $ pack $ show err
