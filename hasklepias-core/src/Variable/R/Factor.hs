{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Variable.R.Factor where

import Data.Aeson (ToJSON (..))
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Variable.R.SEXP

-- | Haskell representation of R's `factor`, either ordered or unordered.
-- Construct with 'factor' or 'ordered', preferably.
data Factor = MkFactor
  { -- | Get the @RTypeRep 'STRSXP@ backing the 'Factor'.
    values :: RTypeRep 'STRSXP,
    -- | Get the levels of the 'Factor'. In R these cannot include 'NA'
    -- values, so 'levels' is of type @Vector Text@ instead of @RTypeRep
    -- 'STRSXP@.
    levels :: V.Vector Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON Factor

-- TODO a candidate for some optimization.

-- | Constructor for 'Factor'. 'as_character' is called on the first argument.
-- User provided 'levels' in the second argument are sorted and made unique
-- with 'VA.sortUniq'.
--
-- Important: As in R, elements of the input vector that do not appear among
-- the 'levels' (after conversion with 'as_character') are marked as invalid,
-- here represented by 'Nothing'. That check has an \(O(nm)\) runtime cost,
-- where n is the length of the input and m the length of the levels.
--
-- To avoid that cost, users can use 'MkConstructor' directly but must
-- themselves ensure all 'values' appear in the 'levels'.
factor :: (AsCharacter a) => a -> V.Vector Text -> Factor
factor rv lvls = MkFactor rv' lvls'
  where
    lvls' = sortUniq lvls
    rv' = V.map op $ as_character rv
    op Nothing = Nothing
    op (Just x) = if V.elem x lvls then Just x else Nothing

-- TODO: would like to support 'ordered', but the previous implementation was misguided.
-- the only way this makes sense is in a case where the user wishes to build levels automatically
-- from sortUniq applied to the values. however, that would conflict with the current factor api,
-- which requires the user to supply the levels explicitly --- which was a specific choice discussed
-- in the MR review for this work as part of v0.30.0. i have removed the 'ordered' api rather than provide
-- something ill-composed that will almost immediately be cruft. --bbrown
