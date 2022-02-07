{-# LANGUAGE OverloadedStrings #-}
module Planning.Decode.Dhall (parseDhallFile, outputTypeString) where

import qualified Data.Text
import qualified Data.Text.IO         (readFile)
import           Data.Void
import qualified Dhall
import           Dhall.Core           (Expr, pretty)
import           Dhall.Marshal.Decode
import           Dhall.Src
import           Planning.Output

instance FromDhall NonNeg
instance FromDhall OutputData

-- Cut-and-paste job from Dhall.Tutorial
-- Dhall.Tutorial shows a better way to do this, but it would require the lens
-- package. Not worth it just for this.

-- TODO: do the same with NonNeg
-- so that type annotations in dhall source work properly

-- OutputData
outputDecoder :: Dhall.Decoder OutputData
outputDecoder = Dhall.auto

outputType :: Expr Src Void
outputType = maximum $ Dhall.expected outputDecoder

outputTypeString :: String
outputTypeString = show $ pretty outputType

-- inject this type into a dhall program string
-- and return parsed Haskell type
-- TODO: txt might need to be a ref to the dhall input file name
parseOutputType :: Data.Text.Text -> IO OutputData
parseOutputType txt = Dhall.input outputDecoder (typedef <> " in " <> txt)
   where typedef = "let OutputData = " <> Data.Text.pack outputTypeString

parseDhallFile :: String -> IO OutputData
parseDhallFile f = do
   x <- Data.Text.IO.readFile f
   parseOutputType x
