{-|
Module      : Hasklepias Types
Description : Re-exports functions from other libraries needed for using
              Hasklepias as a standalone import.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# OPTIONS_HADDOCK hide #-}

module Hasklepias.ReexportsUnsafe
  (

    -- ** Re-exports of (potentially) unsafe functions
    module Data.Aeson
  , module GHC.Exts
  , module GHC.IO
  , module Lens
  , module Test.Tasty
  , module Test.Tasty.HUnit
  , module Witch
  , naturalToInt
  ) where

import           Data.Aeson                     ( ToJSON(..)
                                                , encode
                                                )
import           Data.Generics.Internal.VL.Lens
                                               as Lens
                                                ( (^.) )
import           Data.Generics.Product         as Lens
                                                ( HasField(field) )
import           Data.Generics.Sum             as Lens
                                                ( AsAny(_As) )
import           GHC.Exts                       ( IsList(fromList) )
import           GHC.IO                         ( IO(..) )
import           GHC.Natural                    ( Natural
                                                , naturalToInteger
                                                )
import           GHC.Num                        ( fromInteger )
import           Test.Tasty              hiding ( after )
import           Test.Tasty.HUnit

import           Witch

naturalToInt :: Natural -> Int
naturalToInt = fromInteger . naturalToInteger
