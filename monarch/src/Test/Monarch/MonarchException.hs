{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Test.Monarch.MonarchException where

import           Control.Exception
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Witch.TryFromException

-- | Catch-all exception for failures at various points in the decoding
-- process, from csv file to dhall to internal haskell type.
data MonarchException where
  InputConversionException ::(Show source, Show tag) => TryFromException source target -> tag -> MonarchException
  OutputConversionException ::(Show source, Show tag) => TryFromException source target -> tag -> MonarchException
  DecodeException ::Text -> MonarchException
  UnhandledException ::(Exception t) => t -> MonarchException

-- NOTE we don't want the show instance of TryFromException, which would
-- require us to carry around a Typeable constraint on 'source'. Just show the
-- exception.
instance Show MonarchException where
  show x = case x of
    InputConversionException (TryFromException s e) tag ->
      (++ show s) $ maybe (conversionExString (show tag) "input") show e
    OutputConversionException (TryFromException s e) tag ->
      (++ show s) $ maybe (conversionExString (show tag) "output") show e
    DecodeException    t -> unpack t
    UnhandledException t -> show t

instance Exception MonarchException

-- utils

conversionExString :: String -> String -> String
conversionExString t s = "\n" ++ t ++ ":\n" ++ "Failed to convert from " ++ s ++ " with value\n\n"
