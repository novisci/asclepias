{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Hygiea.Internal.Dhall where

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

   {- UTILS -}
type DhallExpr = Dhall.Core.Expr Src Void

encoderText :: Dhall.Encoder a -> a -> T.Text
encoderText x = T.pack . show . pretty . Dhall.embed x

encoderTypeText :: Dhall.Encoder a -> T.Text
encoderTypeText = T.pack . show . pretty . Dhall.declared

decoderTypeText :: Dhall.Decoder a -> T.Text
decoderTypeText = T.pack . show . pretty . maximum . Dhall.expected

-- TODO clean up these parsers a bit depending on need
parseDhallFileWith
  :: (T.Text -> IO (Expr Src Void)) -> FilePath -> IO (Expr Src Void)
parseDhallFileWith parser file = parser =<< Data.Text.IO.readFile file

parseDhallFile :: FilePath -> IO (Expr Src Void)
parseDhallFile = parseDhallFileWith Dhall.inputExpr

-- alias, fixing Alternative f as Maybe a
-- TODO i want this to be Either
tryParseRawInput :: Dhall.Decoder a -> Expr Src Void -> Maybe a
tryParseRawInput = Dhall.rawInput

-- inject type a into a dhall program string and return decoded Haskell type
-- TODO name handling is ham-handed. grab name from the object itself?
parseDecodeWithType :: T.Text -> Dhall.Decoder a -> T.Text -> IO a
parseDecodeWithType name d program = Dhall.input
  d
  (typedef <> " in " <> program)
  where typedef = "let " <> name <> " = " <> decoderTypeText d

  {- MAP -}

  {- Map v Decoders 

      NOTES:
      * Map v intentionally is not an instance of FromDhall. It differs from
      the typical instance in that we cannot construct the Dhall type from the
      type Map v alone. The entire purpose of Map v is to give us a flat
      structure that can be converted to a Dhall record type without a-priori
      knowledge of what the field names and types are. The latter can be
      provided with actual data, or with a schema.

      -}

-- NOTE: Here you must specify the record names, since 'expected' determines the
-- shape of the decoded object. failures happen at runtime, as usual for dhall

-- | Build a @(Decoder (Map v)) provided a decoder for @v and a list of key
-- names for @Map. You should probably use @decodeMapSchema instead.
decodeMapWith :: Dhall.Decoder v -> [T.Text] -> Dhall.Decoder (Map v)
decodeMapWith decodeVal names = Dhall.Decoder extractOut expectedOut
 where
  extractOut (RecordLit kvs) =
    Map
      .   Dhall.Map.toMap
      <$> traverse (Dhall.extract decodeVal . Dhall.Core.recordFieldValue) kvs
  extractOut expr = Dhall.typeError expectedOut expr
  expectedOut =
    (Record . Dhall.Map.fromList)
      .   (\v -> Prelude.map (, v) names)
      .   Dhall.Core.makeRecordField
      <$> Dhall.expected decodeVal

-- | Build a @(Decoder (Map v)) using the generically derived decoder for @v,
-- provided a list of expected keys. You should probably use
-- @decodeMapSchemaAuto instead.
decodeMap :: (Dhall.FromDhall v) => [T.Text] -> Dhall.Decoder (Map v)
decodeMap = decodeMapWith Dhall.auto

-- | Decode a @(Map v) from Dhall program text and a list of expected keys. You
-- should probably use @mapInputSchema instead.
mapInput :: (Dhall.FromDhall v) => [T.Text] -> T.Text -> IO (Map v)
mapInput names = Dhall.input (decodeMap names)

-- TODO fail with something other than text
-- TODO failure if schema is not record. right now this is basically OK because
-- extractOut implicitly requires this, but the dhall error will be confusing.
-- NOTE: this is awkward because we'd like to grab decodeVal from the schema.
-- See notes in the Atomic module on alternate representations, which would
-- provide that ability.

-- |  Build a @(Decoder (Map v)) using a provided decoder for @v. Unlike with
-- @decodeMapWith, the map key names are provided in a Dhall record schema.
-- This custom decoder is necessary because Dhall by default decodes to @Map
-- for Dhall lists of records with `mapKey` and `mapValue` fields. Here, we use
-- @Map as a container for a more general flat structure with named fields, as
-- one might find in tabular format. As in the `dhall-csv`, the most natural
-- way to specify such a structure in Dhall is via Records. Therefore we write
-- a custom Decoder to a @Map from a Dhall Record.
decodeMapSchema :: Dhall.Decoder v -> DhallExpr -> Dhall.Decoder (Map v)
decodeMapSchema decodeVal schema = Dhall.Decoder extractOut expectedOut
 where
  extractOut (RecordLit kvs) =
    Map
      .   Dhall.Map.toMap
      <$> traverse (Dhall.extract decodeVal . Dhall.Core.recordFieldValue) kvs
  extractOut expr = Dhall.typeError expectedOut expr
  expectedOut = pure schema

-- | Build a @(Decoder (Map v)) using the generically derived decoder for @v,
-- provided a Dhall Record schema.
decodeMapSchemaAuto :: (Dhall.FromDhall v) => DhallExpr -> Dhall.Decoder (Map v)
decodeMapSchemaAuto = decodeMapSchema Dhall.auto

-- | Decode a @(Map v) from Dhall program text and a Dhall Record schema.
mapInputSchema :: (Dhall.FromDhall v) => DhallExpr -> T.Text -> IO (Map v)
mapInputSchema = Dhall.input . decodeMapSchema Dhall.auto

