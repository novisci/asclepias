{-# LANGUAGE NoImplicitPrelude #-}
module Test.Monarch.TestMap
  ( Map(..)
  , TestMap(..)
  , TestVal(..)
  , lookup
  , fromList
  , toList
  -- Atomic items, in support of the Map
  , TestAtomic(..)
  , Atomizable
  ) where

import           Test.Monarch.Internal.Map      ( Map(..)
                                                , TestMap(..)
                                                , fromList
                                                , lookup
                                                , toList
                                                )

import           Test.Monarch.Internal.Atomic   ( Atomizable
                                                , TestAtomic(..)
                                                , TestVal(..)
                                                )

import           Test.Monarch.Internal.EventDataTheory
