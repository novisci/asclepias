{- Internal Map type provides the glue between user input, dhall and project programmer types. Should not be used directly. -}


{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Map.Internal where

import qualified Data.Map.Strict               as SMap
import           Data.Text                      ( Text
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

  {- Map v -}

newtype Map v = Map (SMap.Map Text v) deriving (Show, Eq)

-- TODO ToDhall

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

-- Utilities to help us read to Dhall exprs and identify Record expressions
type DhallExpr = Dhall.Core.Expr Src Void

parseDhallFile :: FilePath -> IO DhallExpr
parseDhallFile f = do
  x <- Data.Text.IO.readFile f
  Dhall.inputExpr x


-- TODO fail with something other than text
-- TODO grab the decoder from the schema, for standard types such as NaturalLit?
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

-- NOTE this still requires all inputs to be the same, so the schema must
-- follow that rule. yuck.
mapInputSchema :: (Dhall.FromDhall v) => DhallExpr -> Text -> IO (Map v)
mapInputSchema = Dhall.input . decodeMapSchema Dhall.auto


-- Map encoders
-- In this case we can use the RecordEncoder Dhall provides
-- Our schema is now a particular Map or a list of names
-- note the Map we are encoding needs to have the same names as those provided

-- TODO look more carefully at the record type
-- skipping 'With' version and using default
injectMap :: forall v. (Dhall.ToDhall v) => [Text] -> RecordEncoder v
injectMap names = RecordEncoder $ Dhall.Map.fromList $ map (, Dhall.inject @v) names


   {- 
      Atomic type 

      NOTES:
      * do not want to use Generic to derive FromDhall, since that would decode to an enum type
      * the FromDhall autoWith implementation ignores the InputNormalizer. We could perhaps change that by following the example of FromDhall (Maybe a) https://hackage.haskell.org/package/dhall-1.40.2/docs/src/Dhall.Marshal.Decode.html#line-312
      -}

data Atomic = AInteger Integer | ANatural Natural | ADouble Double | ABool Bool | AText Text
   deriving (Show, Eq)

instance Dhall.FromDhall Atomic where
  autoWith _ = Decoder extractOut expectedOut
    -- TODO we're using GHC natural in Atomic and NaturalLit references the
    -- reexported version. Does that matter?
   where
    extractOut (Dhall.Core.IntegerLit x) = pure $ AInteger x
    extractOut (Dhall.Core.NaturalLit x) = pure $ ANatural x
    extractOut (Dhall.Core.DoubleLit x) =
      pure $ ADouble $ Dhall.Core.getDhallDouble x
    extractOut (Dhall.Core.BoolLit x) = pure $ ABool x
    -- TODO Chunks? look into it. miht need to concat
    extractOut (Dhall.Core.TextLit (Dhall.Core.Chunks _ x)) = pure $ AText x
    extractOut expr = Dhall.typeError expectedOut expr
    -- TODO following from the Result instance, but is it OK in this case?
    -- I don't understand how the expected argument works, apparantly
    -- https://hackage.haskell.org/package/dhall-1.40.2/docs/src/Dhall.Marshal.Decode.html#line-366
    expectedOut = pure "atomic"

type AtomicMap = Map Atomic

atomicMapInputSchema :: DhallExpr -> Text -> IO AtomicMap
atomicMapInputSchema = mapInputSchema



   {- AtomicOrUnion 
       Atomic but with a Union variant mimicking Dhall.Core.Union
       -}

   {- TODO trickyness
data AtomicOrUnion = Atomic Atomic | Union (Map (Maybe Atomic))
   deriving (Show, Eq)

instance Dhall.FromDhall AtomicOrUnion where
   autoWith _ = Decoder extractOut expectedOut
      where extractOut (Dhall.Core.Union m) = Map . Dhall.Map.toMap <$> traverse (extract Dhall.auto) m
            extractOut expr = Dhall.typeError expectedOut expr
            expectedOut = pure "atomicorunion"

-}
