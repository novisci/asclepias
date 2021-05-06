{-|
Module      : Generate arbitrary events
Description : Functions for generating arbitrary events
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE OverlappingInstances #-}

module Hasklepias.Types.Context.Arbitrary() where

import Test.QuickCheck
    ( Arbitrary(arbitrary), elements, sublistOf ) 
-- ( Arbitrary(arbitrary, shrink), elements )
import GHC.Base(Int, (.), ($), liftM2, liftM, fmap )
import Hasklepias.Types.Context (
     Concept
    , Concepts
    , Context
    , context
    , packConcepts
    , packConcept)
import Data.List (map)
import Data.Set ( fromList )


conceptChoices :: [Concept]
conceptChoices = map packConcept ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J"]

instance Arbitrary Concept where
    arbitrary = elements conceptChoices

instance Arbitrary Context where
    arbitrary = fmap (context . fromList) (sublistOf conceptChoices)

-- instance Arbitrary Concepts where
--     arbitrary = fmap fromList (sublistOf conceptChoices)

