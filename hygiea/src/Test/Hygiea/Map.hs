{-# LANGUAGE NoImplicitPrelude #-}
module Test.Hygiea.Map
  ( Map(..)
  , TestMap(..)
  , TestVal(..)
  , lookup
  , fromList
  , toList
  -- Atomic items, in support of the Map
  , TestAtomic
  ) where

import           Test.Hygiea.Internal.Map       ( Map(..)
                                                , TestMap(..)
                                                , fromList
                                                , lookup
                                                , toList
                                                )

import           Test.Hygiea.Internal.Atomic    ( TestAtomic, TestVal )
