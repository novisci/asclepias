{-# LANGUAGE TypeApplications #-}
module Main
  ( main
  ) where

import           Examples.Simple                ( ProjEvent
                                                , ProjOccurrence
                                                )
import           Monarch
import           System.FilePath

-- TODO test suite should have its own case generators
projPath :: String
projPath = "src/Examples"

inputCsv, outputCsv :: String
inputCsv = projPath </> "input.csv"
outputCsv = replaceFileName inputCsv "output.csv"

inputDhall, outputDhall :: String
inputDhall = projPath </> "input.dhall"
outputDhall = replaceFileName inputCsv "output.dhall"

myRoutine :: TestRoutine
myRoutine = Golden (MkRoutineElem @[ProjEvent] inputCsv inputDhall)
                   (MkRoutineElem @[ProjOccurrence] outputCsv outputDhall)

tests :: TestTree
tests = testGroup "Examples.Simple"
                  [monarchTest "good" myRoutine
-- TODO include tests expected to fail
--  , monarchTest "bad"             myBadRoutine
--  , monarchTest "ugly"            myMisspecRoutine
--  , monarchTest "conversion fail" myRoutine2
                                               ]

main :: IO ()
main = defaultMain tests
