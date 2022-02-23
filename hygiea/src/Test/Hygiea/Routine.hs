{-|
   -}

--{-# LANGUAGE GADTs #-}

module Test.Hygiea.Routine where

import Data.Proxy

-- TODO makes no sense. will possibly want multiple test functions for an input
-- output pair, even for the same data
data Routine input output = RoutineInput (RoutineElem input) | RoutineOutput (RoutineElem output)

