{-# LANGUAGE NoImplicitPrelude #-}
module Test.Monarch.TestMap
  ( Map(..)
  , TestMap(..)
  , TestVal(..)
  , TestAtomic(..)
  , Atomizable
  , lookup
  , fromList
  , toList
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

-- NOTE: This import is necessary for its TryFrom instances
import           Test.Monarch.Internal.EventDataTheory
