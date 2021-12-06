{-|
Module      : Define and evaluate Features 
Description : Defines the Feature type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}
-- {-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}

module Features.Compose
  (
  -- *** Features and FeatureData
    FeatureData
  , MissingReason(..)
  , Feature
  , F
  , FeatureN
  , featureDataL
  , featureDataR
  , missingBecause
  , makeFeature
  , getFeatureData
  , getFData
  , getData
  , getDataN
  , getNameN
  , nameFeature

  -- *** Feature Definitions
  , Definition(..)
  , Define(..)
  , DefineA(..)
  , Def

  --- *** Evalution of Definitions
  , eval
  ) where

import safe      Control.Applicative            ( (<$>)
                                                , Applicative(..)
                                                , liftA3
                                                )
import safe      Control.Monad                  ( (=<<)
                                                , Functor(..)
                                                , Monad(..)
                                                , join
                                                , liftM
                                                , liftM2
                                                , liftM3
                                                , liftM4
                                                , liftM5
                                                )
import safe      Data.Either                    ( Either(..) )
import safe      Data.Eq                        ( Eq(..) )
import safe      Data.Foldable                  ( Foldable(foldr)
                                                , fold
                                                )
import safe      Data.Function                  ( ($)
                                                , (.)
                                                , id
                                                )
import safe      Data.List                      ( (++)
                                                , concat
                                                , transpose
                                                )
import safe      Data.Proxy                     ( Proxy(..) )
import safe      Data.Text                      ( Text
                                                , pack
                                                )
import safe      Data.Traversable               ( Traversable(..) )
import safe      GHC.Generics                   ( Generic )
import safe      GHC.Show                       ( Show(show) )
import safe      GHC.TypeLits                   ( KnownSymbol
                                                , Symbol
                                                , symbolVal
                                                )

-- | Type synonym for 'Feature'.
type F n a = Feature n a

-- | Type synonym for 'Definition'.
type Def d = Definition d

{- | 
Defines the reasons that a @'FeatureData'@ value may be missing. Can be used to
indicate the reason that a @'Feature'@'s data was unable to be derived or does
not need to be derived. 
-}
{- tag::missingReason[] -}
data MissingReason =
    InsufficientData -- ^ Insufficient information available to derive data.
  | Other Text -- ^ User provided reason for missingness
{- end::missingReason[] -}
  deriving (Eq, Show, Generic)

{- | 
The @FeatureData@ type is a container for an (almost) arbitrary type @d@ that can
have a "failed" or "missing" state. The failure is represented by the @'Left'@ of 
an @'Either'@, while the data @d@ is contained in the @'Either'@'s @'Right'@.

To construct a successful value, use @'featureDataR'@. A missing value can be 
constructed with @'featureDataL'@ or its synonym @'missingBecause'@.

-}
{- tag::featureData[] -}
newtype FeatureData d = MkFeatureData {
    getFeatureData :: Either MissingReason d  -- ^ Unwrap FeatureData.
  }
{- end::featureData[] -}
  deriving (Eq, Show, Generic)

-- | Creates a non-missing 'FeatureData'. Since @'FeatureData'@ is an instance of
-- @'Applicative'@, @'pure'@ is also a synonym of for @'featureDataR'@.
-- 
-- >>> featureDataR "aString"
-- MkFeatureData (Right "aString")
-- >>> featureDataR (1 :: P.Int)
-- MkFeatureData (Right 1)
-- 
-- >>> featureDataR ("aString", (1 :: P.Int))
-- MkFeatureData (Right ("aString",1))
--
featureDataR :: d -> FeatureData d
featureDataR = MkFeatureData . Right

-- | Creates a missing 'FeatureData'.
-- 
-- >>> featureDataL (Other "no good reason") :: FeatureData P.Int
-- MkFeatureData (Left (Other "no good reason"))
--
-- >>> featureDataL (Other "no good reason") :: FeatureData Text
-- MkFeatureData (Left (Other "no good reason"))
--
featureDataL :: MissingReason -> FeatureData d
featureDataL = MkFeatureData . Left

-- | A synonym for 'featureDataL'.
missingBecause :: MissingReason -> FeatureData d
missingBecause = featureDataL

{- FeatureData instances -}

-- | Transform ('fmap') @'FeatureData'@ of one type to another.
--
-- >>> x = featureDataR (1 :: P.Int)
-- >>> :type x
-- >>> :type ( fmap show x )
-- x :: FeatureData Int
-- ( fmap show x ) :: FeatureData String
-- 
-- Note that 'Left' values are carried along while the type changes:
--
-- >>> x = ( featureDataL InsufficientData ) :: FeatureData P.Int
-- >>> :type x
-- >>> x
-- >>> :type ( fmap show x )
-- >>> fmap show x 
-- x :: FeatureData Int
-- MkFeatureData {getFeatureData = Left InsufficientData}
-- ( fmap show x ) :: FeatureData String
-- MkFeatureData {getFeatureData = Left InsufficientData}
--
instance Functor FeatureData where
  fmap f (MkFeatureData x) = MkFeatureData (fmap f x)

instance Applicative FeatureData where
  pure = featureDataR
  liftA2 f (MkFeatureData x) (MkFeatureData y) = MkFeatureData (liftA2 f x y)

instance Monad FeatureData where
  (MkFeatureData x) >>= f = case fmap f x of
    Left  l -> MkFeatureData $ Left l
    Right v -> v

instance Foldable FeatureData where
  foldr f x (MkFeatureData z) = foldr f x z

instance Traversable FeatureData where
  traverse f (MkFeatureData z) = MkFeatureData <$> traverse f z

{- | 
The @'Feature'@ is an abstraction for @name@d @d@ata, where the @name@ is a
*type*. Essentially, it is a container for @'FeatureData'@ that assigns a @name@
to the data.

Except when using @'pure'@ to lift data into a @Feature@, @Feature@s can only be
derived from other @Feature@ via a @'Definition'@.
-}
{- tag::feature[] -}
newtype (KnownSymbol name) => Feature name d =
  MkFeature  ( FeatureData d )
{- end::feature[] -}
  deriving (Eq)

-- | Gets the 'FeatureData' from a 'Feature'.
getFData :: Feature name d -> FeatureData d
getFData (MkFeature d) = d

-- | A utility for constructing a @'Feature'@ from @'FeatureData'@.
-- Since @name@ is a type, you may need to annotate the type when using this
-- function.
--
-- >>> makeFeature (pure "test") :: Feature "dummy" Text
-- "dummy": MkFeatureData {getFeatureData = Right "test"}
--
makeFeature :: (KnownSymbol name) => FeatureData d -> Feature name d
makeFeature = MkFeature

-- | A utility for getting the (inner) @'FeatureData'@ content of a @'Feature'@.
getData :: Feature n d -> Either MissingReason d
getData (MkFeature x) = getFeatureData x

{- Feature instances -}
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
  (MkFeature x) >>= f = case fmap f x of
    MkFeatureData (Left  l) -> MkFeature $ MkFeatureData (Left l)
    MkFeatureData (Right r) -> r

{- |
The @'FeatureN'@ type is similar to @'Feature'@ where the @name@ is included
as a @Text@ field. This type is mainly for internal purposes in order to collect
@Feature@s of the same type @d@ into a homogeneous container like a @'Data.List'@.
-}
data FeatureN d = MkFeatureN
  { getNameN :: Text  -- ^ Get the name of a @FeatureN@.
  , getDataN :: FeatureData d -- ^ Get the data of a @FeatureN@
  }
  deriving (Eq, Show)

-- | A utility for converting a @'Feature'@ to @'FeatureN'@.
nameFeature
  :: forall name d . (KnownSymbol name) => Feature name d -> FeatureN d
nameFeature (MkFeature d) = MkFeatureN (pack $ symbolVal (Proxy @name)) d

{- | A @Definition@ can be thought of as a lifted function. Specifically, the
@'define'@ function takes an arbitrary function (currently up to three arguments)
and returns a @Defintion@ where the arguments have been lifted to a new domain.

For example, here we take @f@ and lift to to a function of @Feature@s.

@
f :: Int -> String -> Bool
f i s 
  | 1 "yes" = True
  | otherwise = FALSE

myFeature :: Definition (Feature "A" Int -> Feature "B" String -> Feature "C" Bool )
myFeature = define f
@

See @'eval'@ for evaluating @Defintions@. 

-}

data Definition d  where
  Pure ::a -> Definition (F n0 a )
  D1  ::(b -> a) -> Definition (F n1 b -> F n0 a)
  D1A ::(b -> F n0 a) -> Definition (F n1 b -> F n0 a)
  D1C  ::(a2 -> a1 -> a)
          -> Definition (F n1 b -> F n02 a2)
          -> Definition (F n1 b -> F n01 a1)
          -> Definition (F n1 b -> F n0 a )
  D2  ::(c -> b -> a) -> Definition (F n2 c -> F n1 b -> F n0 a)
  D2A ::(c -> b -> F n0 a) -> Definition (F n2 c -> F n1 b -> F n0 a)
  D2C  ::(a2 -> a1 -> a)
          -> Definition (F n2 c -> F n1 b -> F n02 a2)
          -> Definition (F n2 c -> F n1 b -> F n01 a1)
          -> Definition (F n2 c -> F n1 b -> F n0 a )
  D3  ::(d -> c -> b -> a) -> Definition (F n3 d -> F n2 c -> F n1 b -> F n0 a)
  D3A ::(d -> c -> b -> F n0 a) -> Definition (F n3 d -> F n2 c -> F n1 b -> F n0 a)
  D3C  ::(a2 -> a1 -> a)
          -> Definition (F n3 d -> F n2 c -> F n1 b -> F n02 a2)
          -> Definition (F n3 d -> F n2 c -> F n1 b -> F n01 a1)
          -> Definition (F n3 d -> F n2 c -> F n1 b -> F n0 a )
  D4  ::(e -> d -> c -> b -> a)
         -> Definition (F n4 e -> F n3 d -> F n2 c -> F n1 b -> F n0 a)
  D4A ::(e -> d -> c -> b -> F n0 a)
           -> Definition (F n4 e -> F n3 d -> F n2 c -> F n1 b -> F n0 a)
  D4C  ::(a2 -> a1 -> a)
          -> Definition (F n4 e -> F n3 d -> F n2 c -> F n1 b -> F n02 a2)
          -> Definition (F n4 e -> F n3 d -> F n2 c -> F n1 b -> F n01 a1)
          -> Definition (F n4 e -> F n3 d -> F n2 c -> F n1 b -> F n0 a )
  D5  ::(f -> e -> d -> c -> b -> a)
         -> Definition (F n5 f -> F n4 e -> F n3 d -> F n2 c -> F n1 b -> F n0 a)
  D5A ::(f -> e -> d -> c -> b -> F n0 a)
           -> Definition (F n5 f -> F n4 e -> F n3 d -> F n2 c -> F n1 b -> F n0 a)
  D5C  ::(a2 -> a1 -> a)
          -> Definition (F n5 f -> F n4 e -> F n3 d -> F n2 c -> F n1 b -> F n02 a2)
          -> Definition (F n5 f -> F n4 e -> F n3 d -> F n2 c -> F n1 b -> F n01 a1)
          -> Definition (F n5 f -> F n4 e -> F n3 d -> F n2 c -> F n1 b -> F n0 a )


{- | Define (and @'DefineA@) provide a means to create new @'Definition'@s via 
@'define'@ (@'defineA'@). The @'define'@ function takes a single function input 
and returns a lifted function. For example,

@
f :: Int -> String -> Bool
f i s 
  | 1 "yes" = True
  | otherwise = FALSE

myFeature :: Definition (Feature "A" Int -> Feature "B" String -> Feature "C" Bool )
myFeature = define f
@

The @'defineA'@ function is similar, except that the return type of the input
function is already lifted. In the example below, an input of @Nothing@ is 
considered a missing state: 

@
f :: Int -> Maybe String -> Feature "C" Bool
f i s 
  | 1 (Just "yes")   = pure True
  | _ (Just _ )      = pure False -- False for any Int and any (Just String)
  | otherwise        = pure $ missingBecause InsufficientData -- missing if no string

myFeature :: Definition (Feature "A" Int -> Feature "B" String -> Feature "C" Bool )
myFeature = defineA f
@

-}
class Define inputs def | def -> inputs where
  define :: inputs -> Definition def

instance Define a (Feature n0 a) where
  define = Pure
instance Define (b -> a) (Feature n1 b -> Feature n0 a) where
  define = D1
instance Define (c -> b -> a) (Feature n2 c -> Feature n1 b -> Feature n0 a) where
  define = D2
instance Define (d -> c -> b -> a) (Feature n3 d -> Feature n2 c -> Feature n1 b -> Feature n0 a) where
  define = D3
instance Define (e -> d -> c -> b -> a) (Feature n4 e -> Feature n3 d -> Feature n2 c -> Feature n1 b -> Feature n0 a) where
  define = D4
instance Define (f -> e -> d -> c -> b -> a) (Feature n5 f -> Feature n4 e -> Feature n3 d -> Feature n2 c -> Feature n1 b -> Feature n0 a) where
  define = D5

-- | See @'Define'@.
class DefineA inputs def | def -> inputs where
  defineA :: inputs -> Definition def

instance DefineA (b -> Feature n0 a) (Feature n1 b -> Feature n0 a) where
  defineA = D1A
instance DefineA (c -> b -> Feature n0 a) (Feature n2 c -> Feature n1 b -> Feature n0 a) where
  defineA = D2A
instance DefineA (d -> c -> b -> Feature n0 a) (Feature n3 d -> Feature n2 c -> Feature n1 b -> Feature n0 a) where
  defineA = D3A
instance DefineA (e -> d -> c -> b -> Feature n0 a) (Feature n4 e -> Feature n3 d -> Feature n2 c -> Feature n1 b -> Feature n0 a) where
  defineA = D4A
instance DefineA (f -> e -> d -> c -> b -> Feature n0 a) (Feature n5 f -> Feature n4 e -> Feature n3 d -> Feature n2 c -> Feature n1 b -> Feature n0 a) where
  defineA = D5A

{- | Evaluate a @Definition@. Note that (currently), the second argument of 'eval'
is a *tuple* of inputs. For example,

@
f :: Int -> String -> Bool
f i s 
  | 1 "yes" = True
  | otherwise = FALSE

myFeature :: Definition (Feature "A" Int -> Feature "B" String -> Feature "C" Bool )
myFeature = define f

a :: Feature "A" Int
a = pure 1

b :: Feature "B" String
b = pure "yes"

c = eval myFeature a b
@

-}

eval :: Definition d -> d
eval d = case d of
  Pure x -> pure x
  D1   f -> \(MkFeature x) -> MkFeature $ fmap f x
  D1A  f -> \(MkFeature x) -> case fmap f x of
    MkFeatureData (Left  l) -> MkFeature $ MkFeatureData (Left l)
    MkFeatureData (Right r) -> r
  D1C f d1 d2 ->
    \x -> MkFeature $ liftA2 f (getFData (eval d1 x)) (getFData (eval d2 x))
  D2  f -> \(MkFeature x) (MkFeature y) -> MkFeature $ liftA2 f x y
  D2A f -> \(MkFeature x) (MkFeature y) -> case liftA2 f x y of
    MkFeatureData (Left  l) -> MkFeature $ MkFeatureData (Left l)
    MkFeatureData (Right r) -> r
  D2C f d1 d2 -> \x y ->
    MkFeature $ liftA2 f (getFData (eval d1 x y)) (getFData (eval d2 x y))
  D3 f ->
    \(MkFeature x) (MkFeature y) (MkFeature z) -> MkFeature $ liftA3 f x y z
  D3A f -> \(MkFeature x) (MkFeature y) (MkFeature z) -> case liftA3 f x y z of
    MkFeatureData (Left  l) -> MkFeature $ MkFeatureData (Left l)
    MkFeatureData (Right r) -> r
  D3C f d1 d2 -> \x y z ->
    MkFeature $ liftA2 f (getFData $ eval d1 x y z) (getFData $ eval d2 x y z)
  D4 f -> \(MkFeature v) (MkFeature x) (MkFeature y) (MkFeature z) ->
    MkFeature $ liftM4 f v x y z
  D4A f -> \(MkFeature v) (MkFeature x) (MkFeature y) (MkFeature z) ->
    case liftM4 f v x y z of
      MkFeatureData (Left  l) -> MkFeature $ MkFeatureData (Left l)
      MkFeatureData (Right r) -> r
  D4C f d1 d2 -> \v x y z -> MkFeature
    $ liftA2 f (getFData $ eval d1 v x y z) (getFData $ eval d2 v x y z)
  D5 f ->
    \(MkFeature u) (MkFeature v) (MkFeature x) (MkFeature y) (MkFeature z) ->
      MkFeature $ liftM5 f u v x y z
  D5A f ->
    \(MkFeature u) (MkFeature v) (MkFeature x) (MkFeature y) (MkFeature z) ->
      case liftM5 f u v x y z of
        MkFeatureData (Left  l) -> MkFeature $ MkFeatureData (Left l)
        MkFeatureData (Right r) -> r
  D5C f d1 d2 -> \u v x y z -> MkFeature
    $ liftA2 f (getFData $ eval d1 u v x y z) (getFData $ eval d2 u v x y z)
