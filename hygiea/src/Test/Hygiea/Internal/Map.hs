{-| 
Module      : Hygiea.Internal.Map
Description : Flat structure for decoding csv to Dhall to Haskell
Copyright   : (c) NoviSci, Inc 2022
License     : BSD3
Maintainer  : bbrown@targetrwe.com
  -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Test.Hygiea.Internal.Map where

import           Data.Bifunctor                 ( first )
import           Data.Coerce                    ( coerce )
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
import           Test.Hygiea.Internal.Atomic
import           Witch.From
import           Witch.TryFrom
import           Witch.TryFromException

  {- Map v -}

newtype Map v = Map (SMap.Map Text v) deriving (Show, Eq)

instance Functor Map where
  fmap f (Map x) = Map (fmap f x)

instance Foldable Map where
  foldMap f (Map x) = foldMap f x

instance Traversable Map where
  traverse f (Map x) = Map <$> traverse f x

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

-- | You need only implement TryFrom (Map v) a to get TryFrom instances for
-- traversables for free.
instance (Traversable t, TryFrom (Map v) a) => TryFrom (t (Map v)) (t a) where
  tryFrom x =
    first (\(TryFromException _ e) -> TryFromException x e) $ traverse (tryFrom @(Map v) @a) x

  {- UTILS and SYNONYMS -}
-- | The primary flat structure housing values to be tested and providing the
-- glue between text input and internal types to be tested. A wrapper for
-- Data.Map.Strict.Map with Text keys and TestAtomic values.

-- TODO once the second one is fully supported, delete the first and all
-- related
type TestMap = Map TestVal

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
