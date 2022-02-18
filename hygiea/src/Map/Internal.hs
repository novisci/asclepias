{- Internal Map type provides the glue between user input, dhall and project
    programmer types. Should not be used directly. The goal here is essentially
    to create a haskell-side version of the csv-side conversions in dhall-csv
    https://hackage.haskell.org/package/dhall-csv-1.0.1/docs/src/Dhall.CsvToDhall.html

    An alternative would be to use NamedRecord (aliasing HashMap ByteString
    ByteString) from the cassava package directly, and write some helper types
    that already implement To/FromNamedRecord. That combined with dhall-csv
    should give similar results. However, we might lose some type safety on the
    haskell side, would need to write parsers etc. I'm not sure yet whether
    that approach or this is better.

    https://hackage.haskell.org/package/cassava-0.5.2.0/docs/Data-Csv.html#t:ToNamedRecord
    -}


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Map.Internal where

import           Data.Functor.Contravariant     ( contramap )
import qualified Data.Map.Strict               as SMap
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text.IO
import           Data.Void                      ( Void )
import qualified Dhall
import qualified Dhall.Core
import           Dhall.Core                     ( Expr(..) )
import qualified Dhall.Map
import           Dhall.Marshal.Decode           ( Decoder(..)
                                                , Expector(..)
                                                )
import           Dhall.Marshal.Encode           ( Encoder(..)
                                                , RecordEncoder(..)
                                                , recordEncoder
                                                )
import           Dhall.Src                      ( Src )
import           GHC.Natural                    ( Natural )
import           Witch.From
import           Witch.TryFrom
import           Witch.TryFromException

  {- Map v -}
-- TODO likelike need to injest a *List* of inputs.
-- TODO need to write a conversion for dhall ListLit input from Csv to this
-- format

newtype Map v = Map (SMap.Map Text v) deriving (Show, Eq)

instance Functor Map where
  fmap f (Map x) = Map (fmap f x)

instance Foldable Map where
  foldMap f (Map x) = foldMap f x

instance Traversable Map where
  traverse f (Map x) = Map <$> traverse f x

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
      .   (\v -> map (, v) names)
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

-- TODO add Date
data TestAtomic = TInteger Integer
    | TNatural Natural
    | TBool Bool
    | TDouble Double
    | TText Text
    deriving (Show, Eq)

-- TODO
-- data TestVal = Atomic TestAtomic | Union (Map (Maybe TestAtomic)) deriving (Show, Eq)

  {- Decode -}

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


  {- CONVERSIONS -}

-- TODO ways around this tediousness?
instance From Integer TestAtomic where
  from = TInteger
instance From Natural TestAtomic where
  from = TNatural
instance From Bool TestAtomic where
  from = TBool
instance From Double TestAtomic where
  from = TDouble
instance From Text TestAtomic where
  from = TText

instance TryFrom Integer TestAtomic where
  tryFrom = Right . from
instance TryFrom Natural TestAtomic where
  tryFrom = Right . from
instance TryFrom Bool TestAtomic where
  tryFrom = Right . from
instance TryFrom Double TestAtomic where
  tryFrom = Right . from
instance TryFrom Text TestAtomic where
  tryFrom = Right . from

-- TODO i really do not like this. search for a better way of representing a
-- collection of inhomogeneous types that is not Dynamic or an existential
-- type, latter of which doesn't play well with dhall
instance TryFrom TestAtomic Integer where
  tryFrom (TInteger x) = Right x
  tryFrom t = Left (TryFromException t Nothing)

instance TryFrom TestAtomic Natural where
  tryFrom (TNatural x) = Right x
  tryFrom t = Left (TryFromException t Nothing)

instance TryFrom TestAtomic Bool where
  tryFrom (TBool x) = Right x
  tryFrom t = Left (TryFromException t Nothing)

instance TryFrom TestAtomic Double where
  tryFrom (TDouble x) = Right x
  tryFrom t = Left (TryFromException t Nothing)

instance TryFrom TestAtomic Text where
  tryFrom (TText x) = Right x
  tryFrom t = Left (TryFromException t Nothing)

-- conveniences
instance From (SMap.Map Text v) (Map v) where
  from = Map
instance From (Map v) (SMap.Map Text v) where
  from (Map x) = x

instance From (Dhall.Map.Map Text v) (Map v) where
  from = Map . Dhall.Map.toMap
instance From (Map v) (Dhall.Map.Map Text v) where
  from (Map x) = Dhall.Map.fromMap x

instance From [(Text, v)] (Map v) where
  from = fromList
instance From (Map v) [(Text, v)] where
  from = toList

  {- UTILS and SYNONYMS -}
type DhallExpr = Dhall.Core.Expr Src Void
type TestMap = Map TestAtomic
type Atomizable v = (TryFrom TestAtomic v, From v TestAtomic)

fromList :: [(Text, v)] -> Map v
fromList = from . SMap.fromList

toList :: Map v -> [(Text, v)]
toList = SMap.toList . from

lookup :: Text -> Map v -> Maybe v
lookup k (Map x) = SMap.lookup k x

insertWith :: (v -> v -> v) -> Text -> v -> Map v -> Map v
insertWith f k val (Map x) = Map $ SMap.insertWith f k val x

insert :: Text -> v -> Map v -> Map v
insert = insertWith const
