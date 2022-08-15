module Monarch
  ( RoutineContext
  , TestRoutine(..)
  , RoutineElem(..)
  , monarchTest
  -- | Types and classes
  -- See Test.Monarch.TestMap for Map utilities
  , MonarchException
  , TestMap
  , TestVal(..)
  , TestAtomic(..)
  , Atomizable
  , Testable
  , ToOutput(..)
  -- | Parsers
  , tryParseRecordsCsv
  , decodeMapSchema
  , decodeMapSchemaAuto
  , parseDhallFile
  , parseDhallFileWith
  -- | Reexports
  , TestTree
  , defaultMain
  , testGroup
  -- | Conversion instances to derive via Generic if
  -- needed for custom types
  , Generic
  , ToDhall
  , FromDhall
  , ToJSON
  , FromJSON
  ) where

import           Test.Monarch.MonarchException
import           Test.Monarch.Parse
import           Test.Monarch.TestMap
import           Test.Monarch.ToOutput
import           Test.Tasty.Monarch

import           Data.Aeson                    (FromJSON, ToJSON)
import           Dhall                         (FromDhall, ToDhall)
import           GHC.Generics                  (Generic)
import           Test.Tasty                    (TestTree, defaultMain,
                                                testGroup)
