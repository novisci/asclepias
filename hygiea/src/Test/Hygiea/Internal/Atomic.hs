{-| 
Module      : Hygiea.Internal.Atomic
Description : Internal representation of a testable value type to be used in Map
Copyright   : (c) NoviSci, Inc 2022
License     : BSD3
Maintainer  : bbrown@targetrwe.com
  -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hygiea.Internal.Atomic where

import           Data.Text                      ( Text )
import qualified Dhall
import qualified Dhall.Core
import           GHC.Natural                    ( Natural )
import           Witch.From
import           Witch.TryFrom
import           Witch.TryFromException

-- | Constraint synonym for types that can be converted to and from TestAtomic.
-- Since intentionally TestAtomic supports only a handful of types, conversions
-- from TestAtomic are fallible.
type Atomizable v = (TryFrom TestAtomic v, From v TestAtomic)

-- | A sum type representing all supported types supported for use in testing.
-- 'Atomic' is to be understood as a loose reference to the fact that only
-- non-container types are supported, not in the sense of atomics in concurrent
-- programming.
--
-- In addition, @TestAtomic provides a means to decode from Dhall an
-- inhomogeneous container of values. @TestAtomic is not the only reasonable
-- means to do so, and in future versions of this package we might replace it
-- with an existential type with proper handlers for selecting Dhall decoders
-- from the schema directly, or to a representation using witnesses as in the
-- `singletons` package. The current approach has several limitations and leads
-- to some fraught choices. See notes in the source code.
--
-- For a demo of that kind of approach, see
-- [map-decode](https://gitlab.novisci.com/bbrown/map-decode).

data TestAtomic = TInteger Integer
    -- TODO add Date, requiring to augment Dhall parser but present in Expr
    | TNatural Natural
    | TBool Bool
    | TDouble Double
    | TText Text
    deriving (Show, Eq)

-- TODO
-- the difficulty in adding unions will again be that we need access to the
-- data, in particular the *names* of the variants. therefore we cannot have a
-- FromDhall instance.
-- data TestVal = Atomic TestAtomic | Union (Map (Maybe TestAtomic)) deriving (Show, Eq)

  {- CONVERSIONS -}
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

-- TODO i really do not like this. see the comments about alternatives above.
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

instance Dhall.FromDhall TestAtomic where
  autoWith _ = Dhall.Decoder extractOut expectedOut
   where
    extractOut (Dhall.Core.IntegerLit x) = pure $ TInteger x
    extractOut (Dhall.Core.NaturalLit x) = pure $ TNatural x
    extractOut (Dhall.Core.DoubleLit x) =
      pure $ TDouble $ Dhall.Core.getDhallDouble x
    extractOut (Dhall.Core.BoolLit x) = pure $ TBool x
    -- TODO Chunks? might need to concat
    extractOut (Dhall.Core.TextLit (Dhall.Core.Chunks _ x)) = pure $ TText x
    extractOut expr = Dhall.typeError expectedOut expr
    -- TODO following from the Result instance of Dhall, but this seems a nasty hack
    -- https://hackage.haskell.org/package/dhall-1.40.2/docs/src/Dhall.Marshal.Decode.html#line-366
    expectedOut = pure "atomic"
