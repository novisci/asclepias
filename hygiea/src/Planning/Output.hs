{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Planning.Output (OutputData(..), NonNeg) where

import           Data.Ratio
import           Data.Text
import           GHC.Generics
import           GHC.Natural
import           Witch.From

  {- Output -}

-- TODO this should be revisited

-- a version of Hasklepias.Misc.CensoredOccurence
-- think of it as bare-bones data, after all is unwrapped and converted to text

-- see the Core/README.adoc for a discussion of why i think a concrete data
-- type, rather than e.g. a heterogeneous list

-- TODO: placeholder for continuous nonneg
newtype NonNeg
  = NonNeg Natural
  deriving (Eq, Generic, Show)

data OutputData
  = MkOutputData
      { getData :: [Text]
      , getTime :: NonNeg
      }
  deriving (Eq, Generic, Show)

-- default conversion
instance From OutputData OutputData where
