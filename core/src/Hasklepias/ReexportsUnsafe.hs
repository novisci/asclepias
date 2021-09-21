{-|
Module      : Hasklepias Types
Description : Re-exports functions from other libraries needed for using
              Hasklepias as a standalone import.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}


module Hasklepias.ReexportsUnsafe
  (

    -- * Re-exports
    module Data.Aeson
  , module GHC.Exts
  , module GHC.IO
  , module Test.Tasty
  , module Test.Tasty.HUnit
  ) where

import           Data.Aeson                     ( ToJSON(..)
                                                , encode
                                                )
import           GHC.Exts                       ( IsList(fromList) )
import           GHC.IO                         ( IO(..) )

-- import GHC.Types                       ( Any )
import           Test.Tasty              hiding ( after )
import           Test.Tasty.HUnit
