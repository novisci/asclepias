{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : Hasklepias Contexts
Description : Defines the Context type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module Hasklepias.Types.Context(
    Context(getConcepts)
  , context
  , emptyContext

  , Concept
  , Concepts
  , HasConcept(..)
) where

import GHC.Base (Eq, Bool, Maybe(..), ($))
import GHC.Show ( Show )
import Data.Semigroup ( Semigroup((<>)) )
import Data.Monoid ( (<>), Monoid(mempty) )
import Data.Text (Text)
import Data.List (any)
import Data.Set (Set, fromList, union, empty, map, toList, member)

-- | A @Context@ consists of three parts: 'concepts', 'facts', and 'source'. 
-- 
-- At this time, 'facts' and 'source' are simply stubs to be fleshed out in 
-- later versions of 'Hasklepias'. 
data Context = Context {
      getConcepts :: Concepts
    , getFacts    :: Maybe Facts
    , getSource   :: Maybe Source
} deriving (Eq, Show)

data Facts  = Facts  deriving (Eq, Show)
data Source = Source deriving (Eq, Show)

instance Semigroup Context where
    x <> y = Context (getConcepts x <> getConcepts y) Nothing Nothing

instance Monoid Context where
    mempty = emptyContext

instance HasConcept Context where
    hasConcept ctxt concept = member concept (getConcepts ctxt)

-- | Smart contructor for Context type
--
-- Creates 'Context' from a list of 'Concept's. At this time, the 'facts' and
-- 'source' are both set to 'Nothing'.
context :: [Concept] -> Context
context x = Context (fromList x) Nothing Nothing

-- | Just an empty Context
emptyContext :: Context
emptyContext = Context mempty Nothing Nothing

-- | Concepts are textual "tags" for contexts.
type Concept  = Text
type Concepts = Set Concept

{- |
The 'HasConcept' typeclass provides predicate functions for determining whether
an 'a' has a concept.
-}
class HasConcept a where
    -- | Does an 'a' have a particular 'Concept'?
    hasConcept  :: a -> Concept -> Bool

    -- | Does an 'a' have any of a list of 'Concept's?
    hasConcepts :: a -> [Concept] -> Bool
    hasConcepts x = any (\c -> x `hasConcept` c) 
