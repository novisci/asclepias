-- placeholder for future testing builders and runners
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Planning.Test (Testable(..), testIt) where

import Planning.Output
import Witch.From

-- think of this as
-- type ToOutput input output = (From input output, From output OutputData)
-- but we might wish to have a separate implementation for From input output
-- unrelated to testing
class (From output OutputData) => Testable input output where
  toOutput :: input -> output

-- A placeholder for a test to show conversion constraints. Plan is to use
-- golden testing with tasty-silver.
testIt :: forall input output. (Testable input output) => input -> output -> Bool
testIt ipt opt = actual == expected
  where expected = from opt :: OutputData
        actual = from @output (toOutput ipt)
