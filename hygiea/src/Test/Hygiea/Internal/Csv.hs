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

-- parse Csv [NamedRecord] into dhall, then from dhall into a with the provided decoder
-- TODO better failure handler
tryParseRecords :: Dhall.Decoder a -> [NamedRecord] -> Either HygieaException [a]
tryParseRecords d rs = joinFold (tryParseRawInput d) $ tryListLitToList es
 where
  es = Dhall.CsvToDhall.dhallFromCsv Dhall.CsvToDhall.defaultConversion expr rs
  expr = maximum $ Dhall.expected d
  joinFold f (Left  err) = Left err
  joinFold f (Right xs ) = foldr op (Right []) xs
   where
    op _ (Left  err) = Left err
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
