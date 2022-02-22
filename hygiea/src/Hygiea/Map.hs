{-# LANGUAGE NoImplicitPrelude #-}
module Hygiea.Map
  ( Map(..)
  , TestMap(..)
  , lookup
  , fromList
  , toList
  -- Atomic items, in support of the Map
  , TestAtomic
  ) where

import           Hygiea.Internal.Map            ( Map(..)
                                                , TestMap(..)
                                                , fromList
                                                , lookup
                                                , toList
                                                )

import           Hygiea.Internal.Atomic         ( TestAtomic )
