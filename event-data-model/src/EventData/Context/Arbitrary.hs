{-|
Module      : Generate arbitrary contexts
Description : Functions for generating arbitrary context 
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Safe #-}

module EventData.Context.Arbitrary
  () where

import           Data.Function                  ( (.) )
import           Data.Functor                   ( Functor(fmap) )
import           Data.List                      ( map )
import           Data.Maybe                     ( Maybe(Nothing) )
import           Data.Set                       ( fromList )
import           EventData.Context              ( Concept
                                                , Concepts
                                                , Context
                                                , context
                                                , packConcept
                                                , packConcepts
                                                , toConcepts
                                                )
import           EventData.Context.Domain       ( Domain(UnimplementedDomain) )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , elements
                                                , sublistOf
                                                )

conceptChoices :: [Concept]
conceptChoices =
  map packConcept ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J"]

instance Arbitrary Concept where
  arbitrary = elements conceptChoices

instance Arbitrary Context where
  arbitrary = fmap
    (\x -> context (UnimplementedDomain ()) ((toConcepts . fromList) x) Nothing)
    (sublistOf conceptChoices)

-- instance Arbitrary Concepts where
--     arbitrary = fmap fromList (sublistOf conceptChoices)

