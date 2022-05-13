{-| 
Module      : Test.Monarch.Internal.Atomic
Description : Internal representation of a testable value type to be used in Map. 
Copyright   : (c) NoviSci, Inc 2022
License     : BSD3
Maintainer  : bbrown@targetrwe.com
  -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monarch.Internal.Atomic where

import           Data.Text                      ( Text
                                                , stripPrefix
                                                , stripSuffix
                                                )
import           Data.Void
import qualified Dhall
import           Dhall                          ( auto
                                                , rawInput
                                                )
import qualified Dhall.Core                    as DC
import qualified Dhall.Map                     as DM
import qualified Dhall.Marshal.Decode          as DD
import qualified Dhall.Marshal.Encode          as DE
import           Dhall.Src                      ( Src )
import           GHC.Exts                       ( fromList
                                                , toList
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Natural                    ( Natural )
import           Witch.From
import           Witch.TryFrom
import           Witch.TryFromException


-- TODO alias should now be renamed

-- | Constraint synonym for types that can be converted to and from @TestVal@.
-- Since intentionally @TestVal@ supports only a handful of types, conversions
-- from @TestVal@ are fallible. 
type Atomizable v = (TryFrom TestVal v)

-- | Internal type giving supported @Atomic@ values.
data TestAtomic = TInteger Integer
    | TNatural Natural
    | TBool Bool
    | TDouble Double
    | TText Text
    deriving (Show, Eq)

-- | A sum type representing all supported types supported for use in testing.
-- @Atomic@ is to be understood as a loose reference to the fact that only
-- non-container, non-recursive types are supported.
--
-- In addition, @TestVal@ provides a means to decode from Dhall an
-- inhomogeneous container of values. @TestVal@ is not the only reasonable
-- means to do so, and in future versions of this package we might replace it,
-- for example with a more direct from Dhall representation using witnesses as
-- in the `singletons` package.
--
-- The @Map@ in @Union@ records the type information of a sum type: its
-- constructors and contained values if any. Since it is used solely for
-- possible type-sensitive equality checks, the Dhall @Expr@ type is kept for
-- convenience.
--
-- For simple types, such as user-defined sum types whose values are supported
-- by @TestAtomic@, the easiest way to implement @TryFrom@ a @TestVal@ is to derive
-- @Generic@ and use the generic @ToDhall@ instance. Though awkward, it leverages
-- Dhall's Generics machinery.

data TestVal = Atomic TestAtomic
             | Union (DM.Map Text (Maybe (DC.Expr Src Void))) (Text, Maybe TestAtomic)
             -- | This variant is included for future use but is not currently
             -- fully supported, as @dhall-csv@ does not recognize lists for
             -- cell values.
             | List [TestAtomic]
             deriving (Show, Eq, Generic)

  {- To/FromDhall Conversions -}
instance Dhall.FromDhall TestAtomic where
  autoWith _ = Dhall.Decoder extractOut expectedOut
   where
    extractOut (DC.IntegerLit x) = pure $ TInteger x
    extractOut (DC.NaturalLit x) = pure $ TNatural x
    extractOut (DC.DoubleLit x) = pure $ TDouble $ DC.getDhallDouble x
    extractOut (DC.BoolLit x) = pure $ TBool x
    extractOut (DC.TextLit (DC.Chunks _ x)) = pure $ TText x
    extractOut expr = Dhall.typeError expectedOut expr
    -- TODO following from the Result instance of Dhall, but this seems bad
    -- https://hackage.haskell.org/package/dhall-1.40.2/docs/src/Dhall.Marshal.Decode.html#line-366
    expectedOut = pure "TestAtomic"

instance Dhall.FromDhall TestVal where
  autoWith _ = Dhall.Decoder extractOut expectedOut
   where
      -- Sum type variant with value
    extractOut (DC.App (DC.Field (DC.Union dx) fs) x) =
      pure $ toUVal dx (DC.fieldSelectionLabel fs) x
    -- There is not a value
    extractOut (DC.Field (DC.Union dx) fs) =
      pure $ toU dx (DC.fieldSelectionLabel fs)
    -- NOTE this is not currently supported in dhallFromCsv
    extractOut (DC.ListLit _ es) =
      List . toList <$> traverse (DD.extract auto) es
    -- Else try for TestAtomic.
    extractOut expr = Atomic <$> Dhall.extract auto expr
    -- utilities
    toUVal dx' f' x = Union dx' (f', rawInput auto x)
    toU dx' f' = Union dx' (f', Nothing)
    -- see TODO in TestAtomic instance
    expectedOut = pure "TestVal"

instance From TestAtomic (DC.Expr Src Void) where
  from (TInteger x) = DC.IntegerLit x
  from (TNatural x) = DC.NaturalLit x
  from (TBool    x) = DC.BoolLit x
  from (TDouble  x) = DC.DoubleLit (DC.DhallDouble x)
  -- NOTE there appears to be no issue in simply plopping all text in the value
  -- field and ignoring the list argument.
  from (TText    x) = DC.TextLit (DC.Chunks [] x)

instance From TestVal (DC.Expr Src Void) where
  from (Atomic x) = from x
  from (Union t (nm, Nothing)) =
    DC.Field (DC.Union t) (DC.FieldSelection Nothing nm Nothing)
  from (Union t (nm, Just v)) = DC.App
    (DC.Field (DC.Union t) (DC.FieldSelection Nothing nm Nothing))
    (from v)
  from (List xs) = DC.ListLit Nothing (fromList $ map from xs)

instance (Dhall.FromDhall a) => TryFrom (DC.Expr Src Void) a where
  tryFrom x = case rawInput auto x of
    Just x  -> Right x
    Nothing -> Left $ TryFromException x Nothing


-- NOTE: tryVia doesn't work because Expr Src Void is FromDhall
instance (Dhall.FromDhall a) => TryFrom TestVal a where
  tryFrom x = case tryFrom @(DC.Expr Src Void) $ (from @TestVal) x of
    Right out -> Right out
    Left  _   -> Left $ TryFromException x Nothing

instance (Dhall.ToDhall a) => TryFrom a TestVal where
  tryFrom = tryViaDhall

tryViaDhall
  :: (Dhall.ToDhall a) => a -> Either (TryFromException a TestVal) TestVal
tryViaDhall x = case rawInput auto $ Dhall.embed Dhall.inject x of
  Just xx -> Right xx
  Nothing -> Left $ TryFromException x Nothing

  {- Other conversions -}

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

instance From Integer TestVal where
  from = Atomic . TInteger
instance From Natural TestVal where
  from = Atomic . TNatural
instance From Bool TestVal where
  from = Atomic . TBool
instance From Double TestVal where
  from = Atomic . TDouble
instance From Text TestVal where
  from = Atomic . TText

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

instance TryFrom TestAtomic Integer where
  tryFrom (TInteger x) = Right x
  tryFrom t            = Left (TryFromException t Nothing)

instance TryFrom TestAtomic Natural where
  tryFrom (TNatural x) = Right x
  tryFrom t            = Left (TryFromException t Nothing)

instance TryFrom TestAtomic Bool where
  tryFrom (TBool x) = Right x
  tryFrom t         = Left (TryFromException t Nothing)

instance TryFrom TestAtomic Double where
  tryFrom (TDouble x) = Right x
  tryFrom t           = Left (TryFromException t Nothing)

instance TryFrom TestAtomic Text where
  tryFrom (TText x) = Right x
  tryFrom t         = Left (TryFromException t Nothing)

instance From TestAtomic TestVal where
  from = Atomic
