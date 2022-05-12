{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Test.Monarch.MonarchException where

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
data MonarchException where
  ConversionException ::TryFromException source target -> MonarchException
  DecodeException ::Text -> MonarchException
  UnhandledException ::Exception t => t -> MonarchException

-- NOTE we don't want the show instance of TryFromException, which would
-- require us to carry around a Typeable constraint on 'source'. Just show the
-- exception.
instance Show MonarchException where
  show x = case x of
    ConversionException (TryFromException _ e) -> case e of
      Just ee -> show e
      Nothing -> "ConversionException"
    DecodeException    t -> unpack t
    UnhandledException t -> show t

instance Exception MonarchException

-- conveniences
instance From (TryFromException source target) MonarchException where
  from = ConversionException
