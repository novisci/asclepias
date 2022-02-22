{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Hygiea.Internal.Csv where

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
import           Hygiea.Internal.Atomic
import           Hygiea.Internal.Map
import           Hygiea.Internal.Dhall

-- dhallFromCsv converts the list of NamedRecord to a ListLit Nothing (Seq (Expr ...))
-- Convert ListLit Nothing (Seq a) to [a] by converting Seq to list
-- TODO Left type other than text
tryListLitToList
  :: (Show b)
  => Either b (Dhall.Core.Expr s a)
  -> Either T.Text [Dhall.Core.Expr s a]
tryListLitToList (Right (ListLit _ s)) = Right $ GHC.Exts.toList s
tryListLitToList (Right _            ) = Left "Not a ListLit"
tryListLitToList (Left  err          ) = Left $ T.pack $ show err

-- parse Csv [NamedRecord] into dhall, then from dhall into a with the provided decoder
-- TODO there's a better solution to the joinFold. traverse not doing what i expected
-- TODO better failure handler
tryParseRecords :: Dhall.Decoder a -> [NamedRecord] -> Either T.Text [a]
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
      Nothing -> Left "Could not parse all records"


-- copy-paste from csv-to-dhall Main
toCsv :: Bool -> FilePath -> IO [NamedRecord]
toCsv hasHeader file = do
  text <- Data.Text.IO.readFile file
  case Dhall.Csv.Util.decodeCsvDefault hasHeader text of
    Left  err -> fail err
    Right csv -> pure csv


