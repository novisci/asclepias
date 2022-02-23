{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Hygiea where

import           Data.Proxy
import           Data.Typeable                  ( Typeable )
import           Test.Hygiea.ToOutput
import           Test.Tasty.Options             ( IsOption(..)
                                                , OptionDescription(..)
                                                , lookupOption
                                                )
import           Test.Tasty.Providers           ( IsTest(..), Result(..), testPassed, testFailed )


-- | Structure wrapping a @csvFile@ path, a
-- corresponding @dhallSchema@ specifying column names
-- and types, and @elemType@, a proxy to the Haskell
-- type to which the csv should be converted by way of
-- @TestMap@. 
data RoutineElem a = MkRoutineElem
  { csvFile     :: String
  , dhallSchema :: String
  , elemType    :: Proxy a
  }
  deriving (Show, Eq)

-- | The test provider structure: a pair of @RoutineElem
-- input@ and @RoutineElem output@ such that @Testable
-- input output@.
data Routine
  = forall input output . (Testable input output) => Routine (RoutineElem input)
                                                             ( RoutineElem
                                                                 output
                                                             )
  deriving Typeable


-- TODO
instance IsTest Routine where
  testOptions = return [Option (Proxy @Framework)]
  run opts routine _ = do
    let framewk = lookupOption opts :: Framework
    runRoutine framewk routine


-- TODO add IGolden for interactive golden test

-- | Test option controlling what kind of test we want to run. For example, the
-- @Golden@ variant will build a non-interactive golden test with
-- [tasty-silver](https://hackage.haskell.org/package/tasty-silver). At
-- present, that is the only supported variant, but the option is included with
-- the expectation that unit or property testing will be supported in the
-- future.
data Framework = Golden
  deriving (Eq, Typeable)

instance IsOption Framework where
  defaultValue = Golden
  parseValue "Golden" = Just Golden
  parseValue _        = Nothing
  optionName = return "framework"
  optionHelp = return "test framework to use, e.g. Golden or Unit"

-- TODO
runRoutine :: Framework -> Routine -> IO Result
runRoutine framewk (Routine input output) = case framewk of 
                                              Golden -> return $ testPassed "" 
