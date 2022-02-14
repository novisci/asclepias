-- placeholder for future testing builders and runners
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Planning.Test
  ( Testable(..)
  , testIt
  , ToOutput(..)
  ) where

import           Map.Internal
import           Planning.Input
import           Planning.Output
import           Witch.From
import           Witch.TryFrom

class ToOutput input output where
  toOutput :: input -> output

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
