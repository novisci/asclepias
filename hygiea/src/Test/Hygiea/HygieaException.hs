{-# LANGUAGE GADTs #-}
module Test.Hygiea.HygieaException where

import Witch.TryFromException
import Control.Exception

-- TODO

-- | Catch-all exception for failures at various points in the decoding
-- process, from csv file to dhall to internal haskell type.
data HygieaException where
  ConversionException :: TryFromException source target -> HygieaException

instance Show HygieaException where
  show x = case x of
             ConversionException _ -> "ConversionException"

instance Exception HygieaException
