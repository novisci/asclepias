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
import           GHC.Natural
import           System.FilePath
import           Test.Monarch.MonarchException
import           Test.Monarch.TestMap
import           Test.Monarch.ToOutput
import           Test.Tasty.Monarch
import           Witch.TryFrom

  {- Project-specific code

      This section gives a sense of how much project-specific code needs to be written to interact with monarch's testing structure. In this case, since outputs and inputs are aliases for CensoredOccurence and Event, the only monarch-specific code is the ToOutput instance.

      See Main.hs for how it might be used in a test.
      -}


  {- Code required for Monarch testing -}

-- defining the conversion
instance ToOutput [ProjEvent] [ProjOccurrence] where
  toOutput = cohortBuilder index

-- specifying the files
projPath :: String
projPath = "monarch/src/Examples"

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
cohortBuilderSingle idx e = event i (whatIsIt c)
 where
  i = getInterval $ getEvent e
  c = getContext e
  whatIsIt c' | end i >= end idx = c' { getFacts = WasAfter }
              | otherwise          = c' { getFacts = WasBefore }

wasAfter :: Event a SumminElse b -> Bool
wasAfter = (== WasAfter) . getFacts . getContext

-- placeholder for some filterMap operation building outcomes
-- make a ProjOccurrence with reason "after", keeping other data, if the index
-- end is <= the event end. index is (0, 1)
cohortBuilder :: Index -> [ProjEvent] -> [ProjOccurrence]
cohortBuilder ix = foldr op []
 where
  op x xs =
    let x' = cohortBuilderSingle ix x in if wasAfter x' then x' : xs else xs

-- Project-specific types

-- Inputs
-- there is nothing to do for hygeia to implement the TryFrom instances, since
-- these alias the generic event, for which constraints are already
-- implemented, and Integer, Text already implement the necessary conversions
data TrueFacts = Awesome | NotAwesome Text deriving (Show, Eq, Generic)
type ProjEvent = Event Text TrueFacts Integer
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

c1, c2 :: Concepts Text
c1 = packConcepts ["yay"]
c2 = packConcepts ["not yay"]

e1, e2 :: ProjEvent
e1 = event (beginerval 0 1) (context c1 f1 Nothing)
e2 = event (beginerval 0 1) (context c2 f1 Nothing)

c1' :: TestVal
c1' = List [TText "yay"]

e1Out :: [ProjOccurrence]
e1Out = cohortBuilder index [e1]

-- note these conversions are never ones the programmer need to do, and the
-- exception handling uses MonarchException not Text
c1'' :: Either Text (Concepts Text)
c1'' = first (const "bad") $ tryFrom @TestVal @(Concepts Text) c1'


  {- ------
    KICK THE TIRES EXERCISES 

    Write monarch test Routines for the following input/output types. Some
    exercises have defined functions to test, some leave that to you.

    To run the tests, you'll at a minimum need to:

    * define the `ToOutput` instance
    * define the appropriate `Routine`
    * create the schema dhall files
    * create the input/output csv files
    * add the tests to the TestTree runner in Main.hs
    
    Some are chosen specifically because BB thinks they will cause problems.
    ------ -}

-- Ex. 1
-- You shouldn't need to define any new conversion instances for these types

type ExOneInput = Event Text Text Natural
type ExOneOutput = Concepts Text

exOneFun :: [ExOneInput] -> [ExOneOutput]
exOneFun = map (getConcepts . getContext)

instance ToOutput [ExOneInput] [ExOneOutput] where
  toOutput = exOneFun


myRoutine2 :: Routine
myRoutine2 = Golden
  (MkRoutineElem @[ExOneInput] (projPath </> "input2.csv")
                               (projPath </> "input2.dhall")
  )
  (MkRoutineElem @[ExOneOutput] (projPath </> "output2.csv")
                                (projPath </> "output2.dhall")
  )


-- Ex. 2
-- Yes > No?

data BeBest = Yes | No deriving (Show, Generic)

type ExTwoInput = Event BeBest Text Integer
type ExTwoOutput = Event BeBest Text Integer

exTwoFun :: [ExTwoInput] -> [ExTwoOutput]
exTwoFun = id

-- Ex. 3

data WhatISay = This Text | That Text deriving (Show, Generic)

type ExThreeInput = Event Text WhatISay Integer
type ExThreeInteger = Event BeBest Text Integer

-- Ex. 4
-- Don't attempt this one. It's for me (BB).
-- Not among the default schema. Need to write a instance TryFrom TestMap ExFourOutput.

type ExFourInput = Interval Double
type ExFourIndex = Interval Double
data ExFourOutput = MkExFourOutput
  { nAfterIx  :: Natural
  , nBeforeIx :: Natural
  }

--exFourFun :: [ExFourInput] -> ExFourOutput
--exFourFun = undefined
