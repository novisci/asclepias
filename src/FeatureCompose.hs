{-|
Module      : Define and evaluate Features 
Description : Defines the Feature type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module FeatureCompose(
    Definition(..)
  , FeatureData
  , Feature
  , Define(..)
  , MissingReason(..)
  , nameFeature
  , FeatureNamed
  -- , define
  , defineA
  , eval
  , featureDataL
  , featureDataR
  , missingBecause
  , makeFeature
  , getFeatureData
  , getData
  , getData'
  , getName'
) where

import safe GHC.Generics               ( Generic )
import safe GHC.Show                   ( Show(show) )
import safe GHC.TypeLits               ( KnownSymbol, Symbol, symbolVal )
import safe Control.Applicative        ( Applicative(..)
                                        , liftA3, (<$>) )
import safe Control.Monad              ( Functor(..), Monad(..)
                                       , (=<<), join, liftM, liftM2, liftM3)
import safe Data.Either                ( Either(..) )
import safe Data.Eq                    ( Eq(..) )
import safe Data.Foldable              ( Foldable(foldr) )
import safe Data.Function              ( ($), (.) )
import safe Data.List                  ( (++) )
import safe Data.Proxy                 ( Proxy(..) )
-- import safe Data.String
import safe Data.Text                  ( Text, pack )
import safe Data.Traversable           ( Traversable(..) )

{-
TODO: describe me
-}
data MissingReason =
    InsufficientData
  | Excluded
  | Other Text
  | Unknown
  deriving (Eq, Show, Generic)

{-
TODO: describe me
-}
newtype FeatureData a = MkFeatureData (Either MissingReason a)
  deriving (Eq, Show, Generic)

-- | Create the 'Right' side of 'FeatureData'.
featureDataR :: d -> FeatureData d
featureDataR = MkFeatureData . Right

-- | Create the 'Left' side of 'FeatureData'.
featureDataL :: MissingReason -> FeatureData d
featureDataL = MkFeatureData . Left

-- | 
missingBecause :: MissingReason -> FeatureData d
missingBecause = featureDataL

getFeatureData :: FeatureData a -> Either MissingReason a
getFeatureData (MkFeatureData x) = x

getData :: Feature n a -> Either MissingReason a
getData (MkFeature x) = getFeatureData x

instance Functor FeatureData where
  fmap f (MkFeatureData x) = MkFeatureData (fmap f x)

instance Applicative FeatureData where
  pure = MkFeatureData . Right
  liftA2 f (MkFeatureData x) (MkFeatureData y) = MkFeatureData (liftA2 f x y)

instance Monad FeatureData where
  (MkFeatureData x) >>= f =
      case fmap f x of
         Left l  -> MkFeatureData $ Left l
         Right v -> v

instance Foldable FeatureData where
  foldr f x (MkFeatureData z) = foldr f x z

instance Traversable FeatureData where
  traverse f (MkFeatureData z) = MkFeatureData <$> traverse f z

{-
TODO: describe me
-}
newtype (KnownSymbol name) => Feature name a = MkFeature (FeatureData a)
  deriving (Eq)

makeFeature :: (KnownSymbol name) =>  FeatureData a -> Feature name a
makeFeature = MkFeature

instance (KnownSymbol name, Show a) => Show (Feature name a) where
  show (MkFeature x) = show (symbolVal (Proxy @name)) ++ ": " ++ show x

instance Functor (Feature name) where
  fmap f (MkFeature x) = MkFeature (fmap f x)

instance Applicative (Feature name) where
  pure x = MkFeature (pure x)
  liftA2 f (MkFeature x) (MkFeature y) = MkFeature (liftA2 f x y)

instance Foldable (Feature name) where
  foldr f x (MkFeature t) = foldr f x t

instance Traversable (Feature name) where
  traverse f (MkFeature x) = MkFeature <$> traverse f x

instance Monad (Feature name) where
   (MkFeature x) >>= f =
        case fmap f x of
          MkFeatureData (Left l)  -> MkFeature $ MkFeatureData (Left l)
          MkFeatureData (Right r) ->  r

data FeatureNamed d = MkFeatureNamed {
        getName' :: Text
      , getData' :: FeatureData d
      } deriving (Eq, Show)

nameFeature :: forall name d . (KnownSymbol name) => Feature name d -> FeatureNamed d
nameFeature (MkFeature d) = MkFeatureNamed (pack $ symbolVal (Proxy @name)) d


{-
TODO: describe me
-}
data Definition d where
  -- D0  :: a -> Definition (f1 a)
  D1  :: (b -> a) -> Definition (f1 b -> f0 a)
  D1A :: (b -> f0 a) -> Definition (f1 b -> f0 a)
  D2  :: (c -> b -> a) -> Definition (f2 c -> f1 b -> f0 a)
  D2A :: (c -> b -> f0 a) -> Definition (f2 c -> f1 b -> f0 a)
  D3  :: (d -> c -> b -> a) -> Definition (f3 d -> f2 c -> f1 b -> f0 a)
  D3A :: (d -> c -> b -> f0 a) -> Definition (f3 d -> f2 c -> f1 b -> f0 a)

class Define inputs def | def -> inputs where
  define :: inputs -> Definition def

class DefineA inputs def | def -> inputs where
  defineA :: inputs -> Definition def

-- instance Define a (FeatureData a) where define = D0
instance Define (b -> a) (FeatureData b -> FeatureData a) where define = D1
instance Define (c -> b -> a) (FeatureData c -> FeatureData b -> FeatureData a) where define = D2
instance Define (d -> c -> b -> a) (FeatureData d -> FeatureData c -> FeatureData b -> FeatureData a) where define = D3

instance DefineA (b -> FeatureData a) (FeatureData b -> FeatureData a) where defineA = D1A
instance DefineA (c -> b -> FeatureData a) (FeatureData c -> FeatureData b -> FeatureData a) where defineA = D2A
instance DefineA (d -> c -> b -> FeatureData a) (FeatureData d -> FeatureData c -> FeatureData b -> FeatureData a) where defineA = D3A

-- instance Define a (Feature n0 a) where define = D0
instance Define (b -> a) (Feature n1 b -> Feature n0 a) where define = D1
instance Define (c -> b -> a) (Feature n2 c -> Feature n1 b -> Feature n0 a) where define = D2
instance Define (d -> c -> b -> a) (Feature n3 d -> Feature n2 c -> Feature n1 b -> Feature n0 a) where define = D3

instance DefineA (b -> Feature n0 a) (Feature n1 b -> Feature n0 a) where defineA = D1A
instance DefineA (c -> b -> Feature n0 a) (Feature n2 c -> Feature n1 b -> Feature n0 a) where defineA = D2A
instance DefineA (d -> c -> b -> Feature n0 a) (Feature n3 d -> Feature n2 c -> Feature n1 b -> Feature n0 a) where defineA = D3A

{-
TODO: describe me
-}
class Eval def args return | def -> args return where
  eval :: Definition def -> args -> return

instance Eval (FeatureData b -> FeatureData a) 
              (FeatureData b)  (FeatureData a) where
  eval (D1 f)  x = fmap f x
  eval (D1A f) x = x >>= f

instance Eval (Feature n1 b -> Feature n0 a)
              (Feature n1 b)  (Feature n0 a) where
  eval (D1 f) (MkFeature x) = MkFeature $ fmap f x
  eval (D1A f) (MkFeature x) =
       case fmap f x of
          MkFeatureData (Left l)  -> MkFeature $ MkFeatureData (Left l)
          MkFeatureData (Right r) -> r


instance Eval (FeatureData c -> FeatureData b -> FeatureData a) 
              (FeatureData c,   FeatureData b) (FeatureData a) where
  eval (D2 f) (x, y) = liftA2 f x y
  eval (D2A f) (x, y) = join (liftA2 f x y)

instance Eval (Feature n2 c -> Feature n1 b -> Feature n0 a)
              (Feature n2 c,   Feature n1 b)  (Feature n0 a)
  where
  eval (D2 f) (MkFeature x, MkFeature y) = MkFeature $ liftA2 f x y
  eval (D2A f) (MkFeature x, MkFeature y) =
      case liftA2 f x y of
          MkFeatureData (Left l)  -> MkFeature $ MkFeatureData (Left l)
          MkFeatureData (Right r) ->  r

instance Eval (FeatureData d -> FeatureData c -> FeatureData b -> FeatureData a)
              (FeatureData d,   FeatureData c,   FeatureData b)  (FeatureData a)
  where
  eval (D3 f) (x, y, z) = liftA3 f x y z
  eval (D3A f) (x, y, z) = join (liftA3 f x y z)

instance Eval (Feature n3 d -> Feature n2 c -> Feature n1 b -> Feature n0 a)
              (Feature n3 d,   Feature n2 c,   Feature n1 b)  (Feature n0 a)
   where
  eval (D3 f) (MkFeature x, MkFeature y, MkFeature z) = MkFeature $ liftA3 f x y z
  eval (D3A f) (MkFeature x, MkFeature y, MkFeature z) =
      case liftA3 f x y z of
          MkFeatureData (Left l)  -> MkFeature $ MkFeatureData (Left l)
          MkFeatureData (Right r) -> r
