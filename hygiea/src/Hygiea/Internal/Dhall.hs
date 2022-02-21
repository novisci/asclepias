{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Hygiea.Internal.Dhall where

import           Data.Csv                       ( NamedRecord )
import qualified Data.Text
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
import           Dhall.Marshal.Decode
import           Dhall.Src
import qualified GHC.Exts                       ( IsList(..) )
import           Hygiea.Internal.Atomic
import           Hygiea.Internal.Map

   {- UTILS -}
type DhallExpr = Dhall.Core.Expr Src Void

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
tryListLitToList (Right (ListLit _ s)) = Right $ GHC.Exts.toList s
tryListLitToList (Right _            ) = Left "Not a ListLit"
tryListLitToList (Left  err          ) = Left $ Data.Text.pack $ show err

-- parse Csv [NamedRecord] into dhall, then from dhall into a with the provided decoder
-- TODO there's a better solution to the joinFold. traverse not doing what i expected
-- TODO better failure handler
tryParseRecords :: Decoder a -> [NamedRecord] -> Either Text [a]
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

  {- ATOMIC -}

instance Dhall.FromDhall TestAtomic where
  autoWith _ = Decoder extractOut expectedOut
   where
    extractOut (Dhall.Core.IntegerLit x) = pure $ TInteger x
    extractOut (Dhall.Core.NaturalLit x) = pure $ TNatural x
    extractOut (Dhall.Core.DoubleLit x) =
      pure $ TDouble $ Dhall.Core.getDhallDouble x
    extractOut (Dhall.Core.BoolLit x) = pure $ TBool x
    -- TODO Chunks? look into it. miht need to concat
    extractOut (Dhall.Core.TextLit (Dhall.Core.Chunks _ x)) = pure $ TText x
    extractOut expr = Dhall.typeError expectedOut expr
    -- TODO following from the Result instance, but is it OK in this case?
    -- I don't understand how the expected argument works, apparantly
    -- https://hackage.haskell.org/package/dhall-1.40.2/docs/src/Dhall.Marshal.Decode.html#line-366
    expectedOut = pure "atomic"


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
decodeMapWith :: Decoder v -> [Text] -> Decoder (Map v)
decodeMapWith decodeVal names = Decoder extractOut expectedOut
 where
  extractOut (RecordLit kvs) =
    Map
      .   Dhall.Map.toMap
      <$> traverse (extract decodeVal . Dhall.Core.recordFieldValue) kvs
  extractOut expr = Dhall.typeError expectedOut expr
  expectedOut =
    (Record . Dhall.Map.fromList)
      .   (\v -> Prelude.map (, v) names)
      .   Dhall.Core.makeRecordField
      <$> expected decodeVal

decodeMap :: (Dhall.FromDhall v) => [Text] -> Decoder (Map v)
decodeMap = decodeMapWith Dhall.auto

mapInput :: (Dhall.FromDhall v) => [Text] -> Text -> IO (Map v)
mapInput names = Dhall.input (decodeMap names)

-- Alternatively, we can pass an Expr which decodes from the appropriate record type
-- would like to use RecordDecoder from Dhall, but that assumes we want to decode into a 
-- Haskell record type. Doing it this way lets us pass a schema read from a dhall file.

-- TODO fail with something other than text
-- TODO failure if schema is not record. right now this is basically OK because
-- extractOut implicitly requires this, but the dhall error will be confusing.
-- NOTE: this is awkward because we'd like to grab decodeVal from the schema
decodeMapSchema :: Decoder v -> DhallExpr -> Decoder (Map v)
decodeMapSchema decodeVal schema = Decoder extractOut expectedOut
 where
  extractOut (RecordLit kvs) =
    Map
      .   Dhall.Map.toMap
      <$> traverse (extract decodeVal . Dhall.Core.recordFieldValue) kvs
  extractOut expr = Dhall.typeError expectedOut expr
  expectedOut = pure schema

-- TODO for v implementing FromDhall
decodeMapSchemaAuto :: (Dhall.FromDhall v) => DhallExpr -> Decoder (Map v)
decodeMapSchemaAuto = decodeMapSchema Dhall.auto

-- TODO Input/Output naming confusion
-- NOTE this still requires all inputs to be the same, so the schema must
-- follow that rule. yuck. this is intended to be used with a type v that wraps
-- various standard types. See the TestVal type.
mapInputSchema :: (Dhall.FromDhall v) => DhallExpr -> Text -> IO (Map v)
mapInputSchema = Dhall.input . decodeMapSchema Dhall.auto

  {- Map v Encoders

     We don't actually need or want encoders here, since Dhall is simply the
     glue converting csv to haskell in a way both configurable and typed.

      NOTES:
      * the Map we are encoding needs to have the same names as those provided
      * follows the recipe in https://hackage.haskell.org/package/dhall-1.40.2/docs/src/Dhall.Marshal.Encode.html#recordEncoder
      -}

-- NOTE the names list determines the names of the Record output type
injectMap :: (Dhall.ToDhall v) => [Text] -> Dhall.Encoder (Map v)
injectMap names = Dhall.Encoder embedOut declaredOut
 where
  embedOut x = Dhall.Core.RecordLit $ embedRecord x
  declaredOut =
    Dhall.Core.Record
      $   Dhall.Core.makeRecordField
      .   Dhall.declared
      <$> encodeTypeRecord
  -- TODO this mess of conversions is because Dhall.Map.Map is not applicative
  embedRecord (Map x) = Dhall.Map.fromList $ toRecordFieldMap <*> SMap.toList x
  -- NOTE intentionally ignoring the input Text labels here
  --toRecordFieldMap :: [(Text, v) -> (Text, Dhall.Core.RecordField Src Void)]
  toRecordFieldMap = map
    (\(k, e) -> fmap (Dhall.Core.makeRecordField . Dhall.embed e))
    encodeTypeRecordList
  encodeTypeRecord     = Dhall.Map.fromList encodeTypeRecordList
  encodeTypeRecordList = encodeTypeMapList names

encodeTypeMapList :: (Dhall.ToDhall v) => [Text] -> [(Text, Dhall.Encoder v)]
encodeTypeMapList = map (, Dhall.inject)

encodeTypeMap
  :: (Dhall.ToDhall v) => [Text] -> Dhall.Map.Map Text (Dhall.Encoder v)
encodeTypeMap = Dhall.Map.fromList . encodeTypeMapList

-- NOTE: we really do want to fail if the schema is not a Record. Else we might
-- end up doing something silly.
-- TODO fail with something other than text
tryInjectMapSchema
  :: (Dhall.ToDhall v)
  => Dhall.Core.Expr Src Void
  -> Either Text (Dhall.Encoder (Map v))
tryInjectMapSchema (Dhall.Core.Record m) = Right
  $ Dhall.Encoder embedOut (Dhall.Core.Record m)
 where
  embedOut (Map x) = Dhall.Core.RecordLit $ Dhall.Map.fromMap $ fmap
    (Dhall.Core.makeRecordField . Dhall.embed Dhall.inject)
    x
tryInjectMapSchema _ = Left "schema is not a record"


  {- TestVal

     Decoding/Encoding inhomogeneous containers to and from Dhall is difficult.
     This sum type wraps all of the supported types we want to allow for tests. 

     We could simple implement the standard To/FromDhall instances for this
     type, augment the Dhall language with this definition, and require all
     test schema to use this type, e.g. { id : TestVal, start : TestVal }.
     Eventually, we could modify the dhall parser to allow the TestVal
     specification to be elided.

     However, that would require the use writing an input flat file to apply
     the appropriate constructor for each variant of this sum type, which I
     view as an unacceptable burden.

     Instead, this implements a custom decoder for TestVal.

     We don't actually need an encoder, and providing one is difficult because
     the `declared` field changes based on the variant of TestAtomic. That
     makes ToDhall unnatural, and if the conversion is needed it would be best
     to do it directly with some function TestVal -> Expr Src Void, as is done
     with NamedRecord in dhall-csv.

     Notes:
     * the decoder here is similar to the fieldConvert utility in dhallToCsv, but simpler. 
     * dhall-csv does not try to implement To/FromDhall for NamedRecord.
     https://hackage.haskell.org/package/dhall-csv-1.0.1/docs/src/Dhall.CsvToDhall.html#local-6989586621679086102
     * this can be thought of as a way to avoid spelling out explicit
     conversions as in dhallToCsv for each type
     * as the TryFrom TestAtomic v conversions show, this approach is less than
     ideal. an alternate path might be to write a typeclass ValueFromDhall that
     acts on values, converting them from dhall to haskell, rather than acting
     on types via Decoder 
      -}


