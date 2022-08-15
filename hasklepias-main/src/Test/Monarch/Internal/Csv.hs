{-|
Module      : Test.Monarch.Internal.Csv
Description : Internal module to parse csv to dhall to TestMap.
Copyright   : (c) NoviSci, Inc 2022
License     : BSD3
Maintainer  : bbrown@targetrwe.com
  -}
{-# LANGUAGE OverloadedStrings #-}
module Test.Monarch.Internal.Csv where

import           Data.Csv                      (NamedRecord)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  (readFile)
import qualified Dhall
import           Dhall.Core                    (Expr (..))
import           Dhall.Csv.Util
import           Dhall.CsvToDhall
import qualified GHC.Exts                      (IsList (..))
import           Test.Monarch.Internal.Dhall
import           Test.Monarch.MonarchException

-- dhallFromCsv converts the list of NamedRecord to a ListLit Nothing (Seq (Expr ...))
-- Convert ListLit Nothing (Seq a) to [a] by converting Seq to list
-- TODO Left type other than text
tryListLitToList
  :: (Show b)
  => Either b (Dhall.Core.Expr s a)
  -> Either MonarchException [Dhall.Core.Expr s a]
tryListLitToList (Right (ListLit _ s)) = Right $ GHC.Exts.toList s
tryListLitToList (Right _) = Left $ DecodeException "Not a ListLit"
tryListLitToList (Left err) = Left $ DecodeException $ T.pack $ show err

-- TODO this works when d is created with dhall of the form of a list of
-- records, even though one might think d provided to tryParseRawInput should
-- be a record only, and d provided to create es should be List of records.
-- It's unclear why providing a List of records to tryParseRecords in the
-- joinFold works. Follow up.
--
-- TODO this conversion generally requires some cleanup.

-- | Parse Csv @[NamedRecord]@ into dhall, then from dhall into a with the
-- provided decoder. Note @d@ should be from dhall specifying a list of
-- records, ie `List { ... }`, with field names corresponding to the column
-- names of the csv.
tryParseRecords
  :: Dhall.Decoder a -> [NamedRecord] -> Either MonarchException [a]
tryParseRecords d rs = joinFold (tryParseRawInput d) $ tryListLitToList es
 where
  es = Dhall.CsvToDhall.dhallFromCsv Dhall.CsvToDhall.defaultConversion expr rs
  -- TODO replace maximum by handling Validation Failure variant. requires
  -- several additional imports.
  expr = maximum $ Dhall.expected d
  joinFold _ (Left  err) = Left err
  joinFold f (Right xs ) = foldr op (Right []) xs
   where
    op _ (Left  err) = Left err
    -- TODO tryParseRawInput should return Either MonarchException a rather
    -- than Maybe a
    op z (Right zs ) = case f z of
      Just zz -> Right (zz : zs)
      Nothing -> Left $ DecodeException "Could not parse all records"

-- TODO replace readFile
-- copy-paste from csv-to-dhall Main
toCsv :: Bool -> FilePath -> IO [NamedRecord]
toCsv hasHeader file = do
  text <- Data.Text.IO.readFile file
  case Dhall.Csv.Util.decodeCsvDefault hasHeader text of
    Left  err -> fail err
    Right csv -> pure csv

-- | Read csv file with provided schema as @"Dhall.Decoder"@, and decode to
-- Haskell type. Note @d@ should be from dhall specifying a list of
-- records, ie `List { ... }`, with field names corresponding to the column
-- names of the csv. The decoder usually would be parsed with
-- @parseDhallFile@ or
-- @parseDhallFileWith@.
tryParseRecordsCsv
  :: Dhall.Decoder a -> FilePath -> IO (Either MonarchException [a])
tryParseRecordsCsv d = fmap (tryParseRecords d) . toCsv True

