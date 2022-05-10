{-# LANGUAGE NoImplicitPrelude #-}
module Test.Hygiea.TestMap
  ( Map(..)
  , TestMap(..)
  , TestVal(..)
  , lookup
  , fromList
  , toList
  -- Atomic items, in support of the Map
  , TestAtomic
  , Atomizable
  ) where

import           Test.Hygiea.Internal.Map       ( Map(..)
                                                , TestMap(..)
                                                , fromList
                                                , lookup
                                                , toList
                                                )

import           Test.Hygiea.Internal.Atomic    ( Atomizable
                                                , TestAtomic
                                                , TestVal
                                                )

import Test.Hygiea.Internal.EventDataTheory
