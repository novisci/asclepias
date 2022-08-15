{-|
Module      : Test.Monarch.Internal.Dhall
Description : Internal module providing parsers and Dhall decoders needed to
marshall csv input to a `TestMap`.
Copyright   : (c) NoviSci, Inc 2022
License     : BSD3
Maintainer  : bbrown@targetrwe.com
  -}
{-# LANGUAGE TupleSections #-}

module Test.Monarch.Internal.Dhall where

import qualified Data.ByteString           as BS
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8')
import           Data.Void                 (Void)
import qualified Dhall
import           Dhall.Core                (Expr (..))
import qualified Dhall.Core
import qualified Dhall.Map
import           Dhall.Src
import           Test.Monarch.Internal.Map

   {- UTILS -}
type DhallExpr = Dhall.Core.Expr Src Void

  {- Kept for future reference but not needed.

-- TODO these are not needed but useful for creating a custom type injected
-- into the dhall language parser. see the Dhall tutorial section on
-- substitutions.
-- https://hackage.haskell.org/package/dhall-1.40.2/docs/Dhall-Tutorial.html#g:24
encoderText :: Dhall.Encoder a -> a -> T.Text
encoderText x = T.pack . show . pretty . Dhall.embed x

encoderTypeText :: Dhall.Encoder a -> T.Text
encoderTypeText = T.pack . show . pretty . Dhall.declared

decoderTypeText :: Dhall.Decoder a -> T.Text
decoderTypeText = T.pack . show . pretty . maximum . Dhall.expected

-- inject type a into a dhall program string and return decoded Haskell type
parseDecodeWithType :: T.Text -> Dhall.Decoder a -> T.Text -> IO a
parseDecodeWithType name d program = Dhall.input
  d
  (typedef <> " in " <> program)
  where typedef = "let " <> name <> " = " <> decoderTypeText d
        -}

readFileViaBytes :: FilePath -> IO T.Text
readFileViaBytes path = do
  b <- BS.readFile path
  case decodeUtf8' b of
    Right txt -> pure txt
    Left  err -> fail $ show err


-- | Parse a .dhall file using a provided parser. This is useful if extending
-- the Dhall language by first injecting a custom type definition, for example.
-- See the [Dhall package Tutorial section on
-- substitutions](https://hackage.haskell.org/package/dhall-1.40.2/docs/Dhall-Tutorial.html#g:24).
parseDhallFileWith
  :: (T.Text -> IO (Expr Src Void)) -> FilePath -> IO (Expr Src Void)
parseDhallFileWith parser file = parser =<< readFileViaBytes file

-- | Parse a .dhall file into an @Expr@ using the Dhall package's @inputExpr@.
parseDhallFile :: FilePath -> IO (Expr Src Void)
parseDhallFile = parseDhallFileWith Dhall.inputExpr

-- TODO Alternative f should be Either MonarchException
tryParseRawInput :: Dhall.Decoder a -> Expr Src Void -> Maybe a
tryParseRawInput = Dhall.rawInput


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
-- shape of the decoded object.

-- | Build a @Decoder (Map v)@ provided a decoder for @v@ and a list of key
-- names for @Map@. You must specify keys as text, so as to determine the
-- expected Dhall type to decode. You should probably use @decodeMapSchema@
-- instead.
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

-- | Build a @Decoder (Map v)@ using the generically derived decoder for @v@,
-- provided a list of expected keys. You should probably use
-- @decodeMapSchemaAuto@ instead.
decodeMap :: (Dhall.FromDhall v) => [T.Text] -> Dhall.Decoder (Map v)
decodeMap = decodeMapWith Dhall.auto

-- | Build a @(Map v) from Dhall program text and a list of expected keys. You
-- should probably use @mapInputSchema instead.
mapInput :: (Dhall.FromDhall v) => [T.Text] -> T.Text -> IO (Map v)
mapInput names = Dhall.input (decodeMap names)

-- |  Build a @Decoder (Map v)@ using a provided decoder for @v@. Unlike
-- @decodeMapWith@, the map key names are provided in a Dhall record schema.
-- This custom decoder is necessary because Dhall by default decodes to @Map@
-- for Dhall lists of records with `mapKey` and `mapValue` fields, which is
-- inconvenient for our purposes. Here, we use @Map@ as a container for a more
-- general flat structure with named fields, as one might find in tabular
-- format. As in the
-- [dhall-csv](https://hackage.haskell.org/package/dhall-csv) package, the most
-- natural way to specify such a structure in Dhall is via the @Record@ variant
-- of an @Expr@. Therefore we write a custom Decoder to a @Map@ from a Dhall
-- @Record@.
--
-- This is intended to be used with @v@ as @TestVal@.
-- In that case, the types of the fields provided in the schema
-- should be the Dhall types corresponding to the @TestVal@ variants.
-- For example, @Atomic (TText)@ should be specified in the schema as @Text@,
-- and @Union@ types should be specified using the Dhall sum-type syntax.
decodeMapSchema :: Dhall.Decoder v -> DhallExpr -> Dhall.Decoder (Map v)
decodeMapSchema decodeVal schema = Dhall.Decoder extractOut expectedOut
 where
  extractOut (RecordLit kvs) =
    Map
      .   Dhall.Map.toMap
      <$> traverse (Dhall.extract decodeVal . Dhall.Core.recordFieldValue) kvs
  extractOut expr = Dhall.typeError expectedOut expr
  expectedOut = pure schema

-- | Build a @Decoder (Map v)@ using the generically derived decoder for @v@,
-- provided a Dhall Record schema.
decodeMapSchemaAuto
  :: (Dhall.FromDhall v) => DhallExpr -> Dhall.Decoder (Map v)
decodeMapSchemaAuto = decodeMapSchema Dhall.auto

-- | Decode a @Map v@ from Dhall program text and a Dhall Record schema.
mapInputSchema :: (Dhall.FromDhall v) => DhallExpr -> T.Text -> IO (Map v)
mapInputSchema = Dhall.input . decodeMapSchema Dhall.auto

