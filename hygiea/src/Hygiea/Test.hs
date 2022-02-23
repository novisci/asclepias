{-|
   -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Hygiea.Test
  ( Testable(..)
  , testIt
  , ToOutput(..)
  ) where

import           Hygiea.Map
import           Witch.From
import           Witch.TryFrom

-- | Class defining the conversion from inputs to outputs to be tested. See
-- Testable.
class ToOutput input output where
  toOutput :: input -> output

-- | Context synonym for an @input and @output pair that can be constructed
-- from the general flat TestMap and run through @toOutput. @input is a Haskell
-- representation of a type serving as input to a function to be tested, given
-- in @toOutput, and @ouput is the resulting type of the conversion. 
--
-- The `TryFrom` instances allow @input and @output to be constructed from
-- textual input via `TestMap`, rather than having the Haskell programmer
-- specify them.
type Testable input output
  = ( TryFrom TestMap input
    , TryFrom TestMap output
    , ToOutput input output
    )

-- A placeholder for a test to show how it might work. Plan is to use golden
-- testing with tasty-silver. Here failure to convert is a test failure (False)
-- Note we're deferring ambiguity in the constraints to use locations.
testIt
  :: forall input output . (Eq output, Testable input output) => TestMap -> TestMap -> Bool
testIt ipt opt = test expected actual
 where
  expected = tryFrom @TestMap @output opt
  actual   = toOutput <$> tryFrom @TestMap @input ipt
  test (Right i) (Right o) = i == o
  test _ _ = False
