{-# LANGUAGE OverloadedStrings #-}
module Planning.Decode.Dhall where

import qualified Data.Text
import qualified Data.Text.IO         (readFile)
import Data.Void (Void)
import qualified Dhall
import           Dhall.Core           (Expr, pretty)
import           Dhall.Marshal.Decode
import           Dhall.Src
import           Planning.Output

   {- UTILS -}
encoderText :: Dhall.Encoder a -> a -> Text
encoderText x = Data.Text.pack . show . pretty . Dhall.embed x

encoderTypeText :: Dhall.Encoder a -> Text
encoderTypeText = Data.Text.pack . show . pretty . Dhall.declared

decoderTypeText :: Dhall.Decoder a -> Text
decoderTypeText = Data.Text.pack . show . pretty . maximum . Dhall.expected

-- TODO clean up these parsers a bit depending on need
parseDhallFileWith :: (Text -> IO (Expr Src Void)) -> FilePath -> IO (Expr Src Void)
parseDhallFileWith parser file = do
  x <- Data.Text.IO.readFile file
  parser x

parseDhallFile :: FilePath -> IO (Expr Src Void)
parseDhallFile = parseDhallFileWith Dhall.inputExpr

-- inject type a into a dhall program string and return decoded Haskell type
-- TODO name handling is ham-handed. grab name from the object itself?
parseDecodeWithType :: Text -> Dhall.Decoder a -> Text -> IO a
parseDecodeWithType name d program = Dhall.input d (typedef <> " in " <> program)
   where typedef = "let " <> name <> " = " <> decoderTypeText d
