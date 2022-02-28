{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Examples.Simple where

-- NOTE dhall imports are just for this example and not needed for a typical
-- project
import           Data.Text                      ( Text )
import           IntervalAlgebra
import           System.FilePath
-- placeholder for actual EDM-theory
import           Test.Hygiea.EventData
import           Test.Hygiea.Map
import           Test.Hygiea.ToOutput
import           Test.Tasty.Hygiea

  {- Project-specific code

      This section gives a sense of how much project-specific code needs to be written to interact with hygiea's testing structure. In this case, since outputs and inputs are aliases for CensoredOccurence and Event, the only hygiea-specific code is the ToOutput instance.
      -}

  {- Code required for Hygiea testing -}

instance ToOutput [ProjEvent] [ProjOccurrence] where
  toOutput = cohortBuilder index

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
myBadRoutine = Golden (MkRoutineElem @[ProjEvent] inputCsv inputDhall)
                   (MkRoutineElem @[ProjOccurrence] badOutputCsv outputDhall)

myMisspecRoutine :: Routine
myMisspecRoutine = Golden (MkRoutineElem @[ProjEvent] badInputCsv inputDhall)
                   (MkRoutineElem @[ProjOccurrence] outputCsv outputDhall)

  {- Other project code -}

-- cohort builder: just some nonsense
cohortBuilderSingle :: ProjInterval -> ProjOccurrence
cohortBuilderSingle = MkCensoredOccurrence "after"

-- placeholder for some filterMap operation building outcomes
-- make a ProjOccurrence with reason "after", keeping other data, if the index
-- end is <= the event end. index is (0, 1)
cohortBuilder :: Index -> [ProjEvent] -> [ProjOccurrence]
cohortBuilder ix = foldr op []
 where
  e = end ix
  op (MkEvent x) xs =
    if end (getInterval x) >= e then cohortBuilderSingle x : xs else xs

-- Project-specific types
-- there is nothing to do for hygeia to implement the TryFrom instances, since
-- these alias the generic event, for which constraints are already
-- implemented, and Integer, Text already implement the necessary conversions
type ProjEvent = Event Text Text Integer
type ProjInterval = PairedInterval (Context Text Text) Integer
type Index = Interval Integer
type ProjOccurrence = CensoredOccurrence Text (Context Text Text) Integer

index :: Index
index = beginervalMoment 0
