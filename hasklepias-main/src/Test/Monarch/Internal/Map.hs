{-|
Module      : Monarch.Internal.Map
Description : Flat structure for decoding csv to Dhall to Haskell
Copyright   : (c) NoviSci, Inc 2022
License     : BSD3
Maintainer  : bbrown@targetrwe.com
  -}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Test.Monarch.Internal.Map where

import           Data.Bifunctor               (first)
import qualified Data.Map.Strict              as SMap
import           Data.Text                    (Text)
import qualified Dhall.Map
import           Test.Monarch.Internal.Atomic
import           Witch.From
import           Witch.TryFrom
import           Witch.TryFromException

  {- Map v -}

-- | The primary flat structure housing values to be tested and providing the
-- glue between text input and internal types to be tested. A wrapper for
-- @Data.Map.Strict.Map@ with @Text@ keys. Here it is used exclusively via
-- @TestMap@.
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

instance (Traversable t, TryFrom (Map v) a) => TryFrom (t (Map v)) (t a) where
  tryFrom x =
    first (\(TryFromException _ e) -> TryFromException x e)
      $ traverse (tryFrom @(Map v) @a) x

  {- UTILS and SYNONYMS -}
-- | The primary flat structure housing values to be tested and providing the
-- glue between text input and internal types to be tested.
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
