{-# LANGUAGE OverloadedStrings #-}
module Planning.Decode.Dhall where

import           Data.Csv                       ( NamedRecord )
import qualified Data.Text
import qualified Data.Text.IO                   ( readFile )
import           Data.Void                      ( Void )
import qualified Dhall
import           Dhall.Core                     ( Expr(..)
                                                , pretty
                                                )
import           Dhall.Csv
import           Dhall.Csv.Util
import           Dhall.CsvToDhall
import           Dhall.Marshal.Decode
import           Dhall.Src
import           GHC.Exts                       ( IsList(..) )
import           Planning.Output

   {- UTILS -}
encoderText :: Dhall.Encoder a -> a -> Text
encoderText x = Data.Text.pack . show . pretty . Dhall.embed x

encoderTypeText :: Dhall.Encoder a -> Text
encoderTypeText = Data.Text.pack . show . pretty . Dhall.declared

decoderTypeText :: Dhall.Decoder a -> Text
decoderTypeText = Data.Text.pack . show . pretty . maximum . Dhall.expected

-- TODO clean up these parsers a bit depending on need
parseDhallFileWith
  :: (Text -> IO (Expr Src Void)) -> FilePath -> IO (Expr Src Void)
parseDhallFileWith parser file = do
  x <- Data.Text.IO.readFile file
  parser x

parseDhallFile :: FilePath -> IO (Expr Src Void)
parseDhallFile = parseDhallFileWith Dhall.inputExpr

-- alias, fixing Alternative f as Maybe a
-- TODO i want this to be Either
tryParseRawInput :: Decoder a -> Expr Src Void -> Maybe a
tryParseRawInput = Dhall.rawInput

-- inject type a into a dhall program string and return decoded Haskell type
-- TODO name handling is ham-handed. grab name from the object itself?
parseDecodeWithType :: Text -> Dhall.Decoder a -> Text -> IO a
parseDecodeWithType name d program = Dhall.input
  d
  (typedef <> " in " <> program)
  where typedef = "let " <> name <> " = " <> decoderTypeText d


  {- CSV -}


-- dhallFromCsv converts the list of NamedRecord to a ListLit Nothing (Seq (Expr ...))
-- Convert ListLit Nothing (Seq a) to [a] by converting Seq to list
-- TODO Left type other than text
tryListLitToList
  :: (Show b)
  => Either b (Dhall.Core.Expr s a)
  -> Either Text [Dhall.Core.Expr s a]
tryListLitToList (Right (ListLit _ s)) = Right $ toList s
tryListLitToList (Right _            ) = Left "Not a ListLit"
tryListLitToList (Left  err          ) = Left $ Data.Text.pack $ show err

-- parse Csv [NamedRecord] into dhall, then from dhall into a with the provided decoder
-- TODO there's a better solution to the joinFold. traverse not doing what i expected
-- TODO better failure handler
tryParseRecords :: Decoder a -> [NamedRecord] -> Either Text [a]
tryParseRecords d rs = joinFold (tryParseRawInput d) $ tryListLitToList es
  where es = Dhall.CsvToDhall.dhallFromCsv Dhall.CsvToDhall.defaultConversion expr rs
        expr = maximum $ Dhall.expected d
        joinFold f (Left err) = Left err
        joinFold f (Right xs) = foldr op (Right []) xs
          where op _ (Left err) = Left err
                op z (Right zs) = case f z of
                                    Just zz -> Right (zz : zs)
                                    Nothing -> Left "Could not parse all records"


-- copy-paste from csv-to-dhall Main
toCsv :: Bool -> FilePath -> IO [NamedRecord]
toCsv hasHeader file = do
  text <- Data.Text.IO.readFile file
  case Dhall.Csv.Util.decodeCsvDefault hasHeader text of
    Left  err -> fail err
    Right csv -> pure csv
