{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoCUSKs #-}
{-# LANGUAGE NoNamedWildCards #-}
{-# LANGUAGE NoStarIsType #-}

-- | Model of R vector 'SEXPTYPE' s with related utilities.
module Variable.R.SEXP where

import Data.Aeson (ToJSON (..))
import Data.Complex (Complex)
import Data.Int (Int32)
import Data.Singletons
import Data.Singletons.TH
import Data.Text (Text, pack)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as VA
import GHC.Float
import GHC.ST (runST)
import Type.Reflection (Typeable, typeRep)
import Variable.Constraints

-- Auto-generation of all the basic types and classes from Data.Singletons.
-- Most language extensions of this model are to support this. See
-- https://github.com/goldfirere/singletons/blob/master/README.md
$(singletons [d|data SEXPTYPE = LGLSXP | INTSXP | REALSXP | CPLSXP | STRSXP | VECSXP|])

deriving instance Show SEXPTYPE

deriving instance Eq SEXPTYPE

{- TYPES -}

-- | The "element" type of an 'RTypeRep'. See 'RTypeRep' for more.
type family SEXPElem (s :: SEXPTYPE) = h | h -> s where
  SEXPElem 'LGLSXP = Bool
  SEXPElem 'INTSXP = Int32
  SEXPElem 'REALSXP = Double
  -- NOTE Complex does not implement ToJSON and therefore cannot be wrapped in
  -- SomeRTypeRep. Its usage for our purposes is questionable.
  SEXPElem 'CPLSXP = Complex Double
  SEXPElem 'STRSXP = Text
  SEXPElem 'VECSXP = (Text, SomeRTypeRep)

-- | 'RTypeRep s' is a Haskell representation of 's', an 'SEXPTYPE', in R.
-- 'Nothing' elements represent `NA` values.
--
-- R SEXP vector types do not distinguish between vectors and their elements,
-- but Haskell does. 'SEXPElem' provides a one-to-one correspondence between
-- 'Vector (Maybe a)' and the supported 'SEXPTYPE' s in R by providing a map
-- between the 'a' and its corresponding Haskell type. 'SEXPElem s' for some
-- 'SEXPTYPE' 's' is chosen such that the conversion from a Haskell value of
-- type 'RTypeRep s' and the R type 's' is as seamless as possible.
--
-- Each 'SEXPElem s' is wrapped in 'Maybe' within an 'RTypeRep s' to allow for
-- typed `NA` values in R. For example, 'Nothing' in an @RTypeRep 'LGLSXP@
-- corresponds to `NA_logical_`.
--
-- A Haskell programmer can work with 'RTypeRep s' types directly, with little
-- to no overhead relative to working with any other 'Data.Vector.Vector (Maybe
-- a)'. Some additional utilities to facilitate conversions are provided as
-- part of the 'AsRTypeRep' class with various aliases, e.g.  'AsLogical'. See
-- the examples.
--
-- To work with 'RTypeRep' directly you will need at a minimum the `DataKinds`
-- extension, and in many cases also the `TypeFamilies` and `FlexibleContexts`
-- extensions.
--
-- ==== __Examples__
--
-- The following mimics R's `mean` function behavior, with an `na.rm` argument
-- removing `NA` values. It is generic over types that can be converted to
-- @RTypeRep 'REALSXP@. The auxiliary function `mean` returns 'NaN :: Double'
-- on an empty vector.
--
-- >>> :set -XOverloadedStrings -XDataKinds -XFlexibleContexts
-- >>> import Variable
-- >>> import qualified Data.Vector as V
-- >>> import GHC.Float (int2Double)
-- >>> :{
-- >>> mean :: V.Vector Double -> Double
-- >>> mean v = V.sum v / int2Double (V.length v)
-- >>>
-- >>> rMean :: (AsNumeric a) => Bool -> a -> RTypeRep 'REALSXP
-- >>> rMean narm = summarizeWith (op narm) . as_numeric
-- >>>   where op True = Just . mean . V.catMaybes
-- >>>         op False = fmap mean . V.sequence
-- >>> :}
-- >>> rMean False (as_integer [True, True]) == V.singleton (Just 1)
-- True
-- >>> rMean False (as_integer [1, 2]) == V.singleton (Just 1.5)
-- True
-- >>> rMean True (as_integer [Just 1, Just 2, Nothing]) == V.singleton (Just 1.5)
-- True
-- >>> rMean False (as_integer [Just 1, Just 2, Nothing]) == V.singleton Nothing
-- True
--
-- ==== __R list representation__
--
-- @RTypeRep 'VECSXP@ is worth a few comments, as it presents the greatest
-- challenge for matching to Haskell's type system:
--
-- * R's list type can be named or unnamed, with the latter case meaning that
-- `names(xs)` is `NULL`. @RTypeRep 'VECSXP@ is a vector of key-value pairs,
-- and therefore each element has a name. Unnamed lists are represented by
-- @RTypeRep 'VECSXP@ with names matching the index of the list.
-- * R lists are inhomogeneous, unlike the atomic vectors. That is captured in
-- the existential type 'SomeRTypeRep', providing the value of each key-value
-- pair within a @RTypeRep 'VECSXP@. Type information about the @RTypeRep s@
-- stored in 'SomeRTypeRep' can be recovered using tools from the `singletons`
-- package. See the "Advanced usage" section.
-- * 'Nothing' elements within @RTypeRep 'VECSXP@ have no equivalent in R and
-- hence probably should not be used.
--
-- ==== __Some caveats__
--
-- * At present, all conversions will pass through JSON, via types
-- in the `aeson` package, and to R via the `jsonlite` package. Therefore, the
-- conversion from 'RTypeRep' to its R counterpart depends on those interim
-- conversions, and in particular on the conversion from numeric types
-- ('INTSXP', 'REALSXP' etc.) to 'Scientific', from the `scientific` package in
-- Haskell.
-- * It is unclear how close the Haskell 'Double' corresponds to an element of
-- R's `numeric` or 'REALSXP' vector. Documentation for both
-- [Haskell](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#t:Double)
-- and R state the types conform to the IEEE double-precision standard, in other words the [binary64
-- format](https://en.wikipedia.org/wiki/Double-precision_floating-point_format).
-- and which has precision of 16 decimal digits, with maximum values of `2e308`.
-- Haskell's documentation is less clear on that point and leaves open the
-- possibility of greater precision.
--
-- ==== __Advanced usage__
--
-- Users can take advantage of the tools from the `singletons` package to
-- extract do type-level programming with 'RTypeRep', including recovering the
-- type erased within 'SomeRTypeRep'. See 'sexpTypeOfErased' and 'as_list' for
-- an example.
type RTypeRep (s :: SEXPTYPE) = Vector (Maybe (SEXPElem s))

-- | Constraints for 'SomeRTypeRep'.
type RTypeRepConstraints (s :: SEXPTYPE) = (SingI s, Typeable s, VariableConstraints (SEXPElem s))

-- | Existential type used within inhomogeneous lists of R vectors, in
-- @RTypeRep 'VECSXP@.
data SomeRTypeRep
  = forall s. (RTypeRepConstraints s) => SomeRTypeRep (RTypeRep s)

instance Show SomeRTypeRep where
  show (SomeRTypeRep x) = show x

instance ToJSON SomeRTypeRep where
  toJSON (SomeRTypeRep x) = toJSON x

{- CONVERSIONS -}

-- NOTE trying to clean this up with constraints like a ~ SEXPElem s leads to
-- overlapping instances in many cases, for reasons that aren't entirely clear
-- to me but are related to the fact that you can't provide a typeclass
-- instance for a type function SEXPElem s directly.  A possible solution to
-- help clean up this section, particularly for the VECSXP conversions, is to
-- write an internal wrapper just for the purpose of defining such instances,
-- with the `as_*` helpers would unwrap. See how that is done in
-- Data.Singletons. --bbrown

-- | Convert Haskell types to 'RTypeRep s'. It is up to the user to define
-- conversions in a manner consistent with R's behavior. 'Nothing' elements of
-- an 'RTypeRep s' correspond to the `NA` value in R, where appropriate.
--
-- Note: It never makes sense to produce a 'Nothing' element within an
-- @RTypeRep 'VECSXP@.
class AsRTypeRep s a where
  as_rtyperep :: a -> RTypeRep s

type AsLogical = AsRTypeRep 'LGLSXP

type AsInteger = AsRTypeRep 'INTSXP

type AsNumeric = AsRTypeRep 'REALSXP

type AsComplex = AsRTypeRep 'CPLSXP

type AsCharacter = AsRTypeRep 'STRSXP

type AsList = AsRTypeRep 'VECSXP

-- Singleton instances
instance AsRTypeRep 'LGLSXP Bool where
  as_rtyperep = V.singleton . Just

instance AsRTypeRep 'LGLSXP (Maybe Bool) where
  as_rtyperep = V.singleton

instance AsRTypeRep 'INTSXP Int32 where
  as_rtyperep = V.singleton . Just

instance AsRTypeRep 'INTSXP (Maybe Int32) where
  as_rtyperep = V.singleton

instance AsRTypeRep 'REALSXP Double where
  as_rtyperep = V.singleton . Just

instance AsRTypeRep 'REALSXP (Maybe Double) where
  as_rtyperep = V.singleton

instance AsRTypeRep 'CPLSXP (Complex Double) where
  as_rtyperep = V.singleton . Just

instance AsRTypeRep 'CPLSXP (Maybe (Complex Double)) where
  as_rtyperep = V.singleton

instance AsRTypeRep 'STRSXP Text where
  as_rtyperep = V.singleton . Just

instance AsRTypeRep 'STRSXP (Maybe Text) where
  as_rtyperep = V.singleton

instance AsRTypeRep 'STRSXP String where
  as_rtyperep = as_character . pack

instance AsRTypeRep 'STRSXP (Maybe String) where
  as_rtyperep = as_character . (pack <$>)

instance AsRTypeRep 'STRSXP Int32 where
  as_rtyperep = as_character . pack . show

instance AsRTypeRep 'STRSXP (Maybe Int32) where
  as_rtyperep = as_character . (pack . show <$>)

instance AsRTypeRep 'STRSXP Bool where
  as_rtyperep = as_character . pack . show

instance AsRTypeRep 'STRSXP (Maybe Bool) where
  as_rtyperep = as_character . (pack . show <$>)

instance AsRTypeRep 'STRSXP Double where
  as_rtyperep = as_character . pack . show

instance AsRTypeRep 'STRSXP (Maybe Double) where
  as_rtyperep = as_character . (pack . show <$>)

instance AsRTypeRep 'STRSXP Int where
  as_rtyperep = as_character . pack . show

instance AsRTypeRep 'STRSXP (Maybe Int) where
  as_rtyperep = as_character . (pack . show <$>)

instance AsRTypeRep 'STRSXP Integer where
  as_rtyperep = as_character . pack . show

instance AsRTypeRep 'STRSXP (Maybe Integer) where
  as_rtyperep = as_character . (pack . show <$>)

-- Identity-ish instances
instance AsRTypeRep 'LGLSXP (Vector Bool) where
  as_rtyperep = V.map Just

instance AsRTypeRep 'LGLSXP [Bool] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'LGLSXP (Vector (Maybe Bool)) where
  as_rtyperep = id

instance AsRTypeRep 'LGLSXP [Maybe Bool] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'INTSXP (Vector Int32) where
  as_rtyperep = V.map Just

instance AsRTypeRep 'INTSXP [Int32] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'INTSXP (Vector (Maybe Int32)) where
  as_rtyperep = id

instance AsRTypeRep 'INTSXP [Maybe Int32] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'INTSXP (Vector Bool) where
  as_rtyperep = V.map (Just . (\b -> if b then 1 else 0))

instance AsRTypeRep 'INTSXP [Bool] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'INTSXP (Vector (Maybe Bool)) where
  as_rtyperep = V.map (fmap (\b -> if b then 1 else 0))

instance AsRTypeRep 'INTSXP [Maybe Bool] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'REALSXP (Vector Double) where
  as_rtyperep = V.map Just

instance AsRTypeRep 'REALSXP [Double] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'REALSXP (Vector (Maybe Double)) where
  as_rtyperep = id

instance AsRTypeRep 'REALSXP [Maybe Double] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'REALSXP (Vector (Maybe Int32)) where
  as_rtyperep = V.map (fmap (int2Double . fromEnum))

instance AsRTypeRep 'REALSXP (Vector Int32) where
  as_rtyperep = V.map (Just . (int2Double . fromEnum))

instance AsRTypeRep 'REALSXP (Vector (Maybe Int)) where
  as_rtyperep = V.map (fmap int2Double)

instance AsRTypeRep 'REALSXP (Vector Int) where
  as_rtyperep = V.map (Just . int2Double)

instance AsRTypeRep 'REALSXP (Vector (Maybe Bool)) where
  as_rtyperep = V.map (fmap (int2Double . fromEnum))

instance AsRTypeRep 'REALSXP (Vector Bool) where
  as_rtyperep = V.map (Just . (int2Double . fromEnum))

instance AsRTypeRep 'CPLSXP (Vector (Complex Double)) where
  as_rtyperep = V.map Just

instance AsRTypeRep 'CPLSXP [Complex Double] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'CPLSXP (Vector (Maybe (Complex Double))) where
  as_rtyperep = id

instance AsRTypeRep 'CPLSXP [Maybe (Complex Double)] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP (Vector Text) where
  as_rtyperep = V.map Just

instance AsRTypeRep 'STRSXP [Text] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP (Vector (Maybe Text)) where
  as_rtyperep = id

instance AsRTypeRep 'STRSXP [Maybe Text] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP (Vector String) where
  as_rtyperep = V.map (Just . pack)

instance AsRTypeRep 'STRSXP [String] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP (Vector (Maybe String)) where
  as_rtyperep = V.map (fmap pack)

instance AsRTypeRep 'STRSXP [Maybe String] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP (Vector Int32) where
  as_rtyperep = V.map (Just . pack . show)

instance AsRTypeRep 'STRSXP (Vector (Maybe Int32)) where
  as_rtyperep = V.map (pack . show <$>)

instance AsRTypeRep 'STRSXP [Int32] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP [Maybe Int32] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP (Vector Bool) where
  as_rtyperep = V.map (Just . pack . show)

instance AsRTypeRep 'STRSXP (Vector (Maybe Bool)) where
  as_rtyperep = V.map (pack . show <$>)

instance AsRTypeRep 'STRSXP [Bool] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP [Maybe Bool] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP (Vector Double) where
  as_rtyperep = V.map (Just . pack . show)

instance AsRTypeRep 'STRSXP (Vector (Maybe Double)) where
  as_rtyperep = V.map (pack . show <$>)

instance AsRTypeRep 'STRSXP [Double] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP [Maybe Double] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP (Vector Int) where
  as_rtyperep = V.map (Just . pack . show)

instance AsRTypeRep 'STRSXP (Vector (Maybe Int)) where
  as_rtyperep = V.map (pack . show <$>)

instance AsRTypeRep 'STRSXP [Int] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP [Maybe Int] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP (Vector Integer) where
  as_rtyperep = V.map (Just . pack . show)

instance AsRTypeRep 'STRSXP (Vector (Maybe Integer)) where
  as_rtyperep = V.map (pack . show <$>)

instance AsRTypeRep 'STRSXP [Integer] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'STRSXP [Maybe Integer] where
  as_rtyperep = as_rtyperep . V.fromList

-- | 'as_list' utility, allowing to match the s in RTypeRep s with that of
-- @Sing s.@ Pattern matching on @Sing s@ is needed here to allow this to be
-- identity in the case of @RTypeRep 'VECSXP@. Otherwise, the type checker
-- cannot infer that @s@ in the input @RTypeRep s@ indeed matches @'VECSXP@.
as_listA :: (RTypeRepConstraints s) => Sing s -> RTypeRep s -> RTypeRep 'VECSXP
as_listA sxp rv = case sxp of
  SVECSXP -> rv
  _ -> V.map (\(ix, v) -> Just (pack $ show ix, SomeRTypeRep $ V.singleton v)) (V.indexed rv)

-- Singleton list instance
-- To create an inhomogeneous list of RTypeRep, do it manally: first convert
-- each to Maybe (Text, SomeRTypeRep).
instance (RTypeRepConstraints s, a ~ SEXPElem s) => AsRTypeRep 'VECSXP (Vector (Maybe a)) where
  as_rtyperep = as_listA (sing @s)

instance (RTypeRepConstraints s, a ~ SEXPElem s) => AsRTypeRep 'VECSXP [a] where
  as_rtyperep = as_listA (sing @s) . V.map Just . V.fromList

-- NOTE: Example of overlapping instance (error appears when invoked).
-- Guess is that the type checker cannot determine that a ~ SEXPElem s is never
-- (a ~ SEXPElem s) => Maybe a.
-- instance (RTypeRepConstraints s, a ~ SEXPElem s) => AsRTypeRep 'VECSXP (Vector a) where
--  as_rtyperep = as_listA (sing @s) . V.map Just

-- Conversions with loss
instance AsRTypeRep 'INTSXP Integer where
  as_rtyperep i
    | i <= mm && i >= mn = as_rtyperep (fromInteger i :: Int32)
    | otherwise = V.singleton Nothing
    where
      mm = toInteger (maxBound :: Int32)
      mn = toInteger (minBound :: Int32)

instance AsRTypeRep 'INTSXP (Maybe Integer) where
  as_rtyperep = \case
    Nothing -> V.singleton Nothing
    Just ii -> as_rtyperep ii

instance AsRTypeRep 'INTSXP (Vector Integer) where
  as_rtyperep = V.concatMap as_rtyperep

instance AsRTypeRep 'INTSXP [Integer] where
  as_rtyperep = as_rtyperep . V.fromList

instance AsRTypeRep 'INTSXP (Vector (Maybe Integer)) where
  as_rtyperep = V.concatMap as_rtyperep

instance AsRTypeRep 'INTSXP [Maybe Integer] where
  as_rtyperep = as_rtyperep . V.fromList

-- | Analogous to R's @as.logical@.
as_logical :: (AsLogical a) => a -> RTypeRep 'LGLSXP
as_logical = as_rtyperep

-- | Analogous to R's @as.integer@.
as_integer :: (AsInteger a) => a -> RTypeRep 'INTSXP
as_integer = as_rtyperep

-- | Analogous to R's @as.numeric@.
as_numeric :: (AsNumeric a) => a -> RTypeRep 'REALSXP
as_numeric = as_rtyperep

-- | Analogous to R's @as.character@. Includes versions to directly construct
-- character vectors from lists or vector containing Show elements.
as_character :: (AsCharacter a) => a -> RTypeRep 'STRSXP
as_character = as_rtyperep

-- | @as_list v@ is analogous to R's @as.list(v)@ for 'v' of the supported
-- @RTypeRep s@. Note R's @...@ syntax is not supported here. The constraints
-- provided are those of 'SomeRTypeRep'.
as_list :: (AsList v) => v -> RTypeRep 'VECSXP
as_list = as_rtyperep

{- UTILITIES -}

-- | Run a routine with 'SomeRTypeRep', whose 'RTypeRep s' type you cannot
-- inspect.
withSomeRTypeRep :: SomeRTypeRep -> (forall s. (RTypeRepConstraints s) => RTypeRep s -> t) -> t
withSomeRTypeRep (SomeRTypeRep sexp) f = f sexp

-- | R has no element-of-vector type, only vector types. This utiltity takes
-- Haskell functions that summarize an R vector, and wrap the output in the
-- appropriate RTypeRep, as a singleton, mimicing what R would do and (I hope)
-- lowering the overhead.
summarizeWith :: (RTypeRep s -> Maybe (SEXPElem s')) -> RTypeRep s -> RTypeRep s'
summarizeWith f = V.singleton . f

-- | Sort a 'RTypeRep' using 'Data.Vector.Algorithms.MergeSort.sort'.
sort :: (Ord (SEXPElem s)) => RTypeRep s -> RTypeRep s
sort = V.modify VA.sort

-- | Sort a 'Vector' and return unique elements using
-- 'Data.Vector.Algorithms.MergeSort.sortUniq'.
sortUniq :: (Ord a) => Vector a -> Vector a
sortUniq v = runST $ do
  mv <- V.thaw v
  mv' <- VA.sortUniq mv
  V.freeze mv'

-- | Return the 'Sing s' associated with a value of 'RTypeRep s'.
singOfRTypeRep :: forall r. (SingI r) => RTypeRep r -> Sing r
singOfRTypeRep _ = sing @r

-- | Return the 'SEXPTYPE' of an 'RTypeRep (s :: SEXPTYPE)'.
--
-- >>> sexpTypeOf (V.fromList [True]) == LGLSXP
-- >>> sexpTypeOf (V.fromList [1 :: Double]) == REALSXP
sexpTypeOf :: forall (r :: SEXPTYPE). (SingI r) => RTypeRep r -> SEXPTYPE
sexpTypeOf _ = fromSing $ sing @r

sexpTypeOfElem :: forall (r :: SEXPTYPE). (SingI r) => SEXPElem r -> SEXPTYPE
sexpTypeOfElem _ = fromSing $ sing @r

-- | Recover the 'Sing r' from 'SomeRTypeRep', which recovers the type
-- information lost in the existential type.
sexpTypeOfErased :: SomeRTypeRep -> SEXPTYPE
sexpTypeOfErased = (`withSomeRTypeRep` sexpTypeOf)

-- | Show the 'SEXPElem r' type of the given 'RTypeRep r'.
showSEXPElemOf :: forall r. (Typeable (SEXPElem r)) => RTypeRep r -> String
showSEXPElemOf _ = show $ typeRep @(SEXPElem r)

-- | 'showSEXPElemOf' converted to 'Text'.
textSEXPElemOf :: forall r. (Typeable (SEXPElem r)) => RTypeRep r -> Text
textSEXPElemOf = pack . showSEXPElemOf

-- | Show the 'r :: SEXPTYPE' of the given 'RTypeRep r'.
showSEXPTYPEOf :: forall (r :: SEXPTYPE). (SingI r) => RTypeRep r -> String
showSEXPTYPEOf = show . sexpTypeOf

-- | 'showSEXPTYPEOf' converted to 'Text'.
textSEXPTYPEOf :: forall r. (SingI r) => RTypeRep r -> Text
textSEXPTYPEOf = pack . showSEXPTYPEOf

textSEXPTYPEOfElem :: forall r. (SingI r) => SEXPElem r -> Text
textSEXPTYPEOfElem = pack . show . sexpTypeOfElem

-- | Show the 'SEXPTYPE' of an underlying 'SomeRTypeRep'.
showSEXPTYPEOfErased :: SomeRTypeRep -> String
showSEXPTYPEOfErased v = withSomeRTypeRep v showSEXPTYPEOf

-- | Show the 'SEXPTYPE' of an underlying 'SomeRTypeRep', as 'Text'.
textSEXPTYPEOfErased :: SomeRTypeRep -> Text
textSEXPTYPEOfErased v = withSomeRTypeRep v textSEXPTYPEOf
