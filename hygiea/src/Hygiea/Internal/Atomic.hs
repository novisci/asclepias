{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hygiea.Internal.Atomic where

import           Data.Text                      ( Text )
import           GHC.Natural                    ( Natural )
import           Witch.From
import           Witch.TryFrom
import           Witch.TryFromException

type Atomizable v = (TryFrom TestAtomic v, From v TestAtomic)

-- TODO add Date
data TestAtomic = TInteger Integer
    | TNatural Natural
    | TBool Bool
    | TDouble Double
    | TText Text
    deriving (Show, Eq)

-- TODO
-- data TestVal = Atomic TestAtomic | Union (Map (Maybe TestAtomic)) deriving (Show, Eq)


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

  {- CONVERSIONS -}
-- TODO i really do not like this. search for a better way of representing a
-- collection of inhomogeneous types that is not Dynamic or an existential
-- type, latter of which doesn't play well with dhall
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
