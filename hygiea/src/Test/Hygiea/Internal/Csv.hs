{-# LANGUAGE OverloadedStrings #-}
module Test.Hygiea.Internal.Csv where

import           Data.Csv                       ( NamedRecord )
import qualified Data.Text as T
import qualified Data.Text.IO                   ( readFile )
import           Data.Void                      ( Void )
import qualified Dhall
import qualified Dhall.Core
import           Dhall.Core                     ( Expr(..)
                                                , pretty
                                                )
import           Dhall.Csv
import           Dhall.Csv.Util
import           Dhall.CsvToDhall
import qualified Dhall.Map
import qualified Dhall.Marshal.Decode          as Decode
import           Dhall.Src
import qualified GHC.Exts                       ( IsList(..) )
import           Test.Hygiea.Internal.Atomic
import           Test.Hygiea.Internal.Map
import           Test.Hygiea.Internal.Dhall
import           Test.Hygiea.HygieaException

-- dhallFromCsv converts the list of NamedRecord to a ListLit Nothing (Seq (Expr ...))
-- Convert ListLit Nothing (Seq a) to [a] by converting Seq to list
-- TODO Left type other than text
tryListLitToList
  :: (Show b)
  => Either b (Dhall.Core.Expr s a)
  -> Either HygieaException [Dhall.Core.Expr s a]
tryListLitToList (Right (ListLit _ s)) = Right $ GHC.Exts.toList s
tryListLitToList (Right _            ) = Left $ DecodeException "Not a ListLit"
tryListLitToList (Left  err          ) = Left $ DecodeException $ T.pack $ show err

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
tryParseRecords :: Dhall.Decoder a -> [NamedRecord] -> Either HygieaException [a]
tryParseRecords d rs = joinFold (tryParseRawInput d) $ tryListLitToList es
 where
  es = Dhall.CsvToDhall.dhallFromCsv Dhall.CsvToDhall.defaultConversion expr rs
  expr = maximum $ Dhall.expected d
  joinFold f (Left  err) = Left err
  joinFold f (Right xs ) = foldr op (Right []) xs
   where
    op _ (Left  err) = Left err
    -- TODO tryParseRawInput should return Either HygieaException a rather
    -- than Maybe a
    op z (Right zs ) = case f z of
      Just zz -> Right (zz : zs)
      Nothing -> Left $ DecodeException "Could not parse all records"

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
-- @"Test.Hygiea.Internal.Dhall".parseDhallFile@ or
-- @"Test.Hygiea.Internal.Dhall".parseDhallFileWith@. 
tryParseRecordsCsv :: Dhall.Decoder a -> FilePath -> IO (Either HygieaException [a])
tryParseRecordsCsv d = fmap (tryParseRecords d) . toCsv True
