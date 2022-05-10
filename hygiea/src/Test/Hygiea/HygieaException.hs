{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Test.Hygiea.HygieaException where

import           Control.Exception
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Witch.From
import           Witch.TryFromException

-- TODO a full pass at the exceptions used throughout, and of the Show
-- instance. messages should be more informative if possible.

-- | Catch-all exception for failures at various points in the decoding
-- process, from csv file to dhall to internal haskell type.
data HygieaException where
  ConversionException ::TryFromException source target -> HygieaException
  DecodeException ::Text -> HygieaException
  UnhandledException ::Exception t => t -> HygieaException

-- NOTE we don't want the show instance of TryFromException, which would
-- require us to carry around a Typeable constraint on 'source'. Just show the
-- exception.
instance Show HygieaException where
  show x = case x of
    ConversionException (TryFromException _ e) -> case e of
      Just ee -> show e
      Nothing -> "ConversionException"
    DecodeException t -> unpack t
    UnhandledException t -> show t

instance Exception HygieaException

-- conveniences
instance From (TryFromException source target) HygieaException where
  from = ConversionException
