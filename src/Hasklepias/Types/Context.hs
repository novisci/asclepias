{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Hasklepias Event Type
Description : Specifies the Event type and related functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module Hasklepias.Types.Context(
    Context(..)
  , context
  , Concept
  , Concepts
  , HasConcept(..)
  , emptyContext
) where

import Data.Text (Text)
import Data.List (union)

-- | A @Context@ consists of three parts: 'concepts', 'facts', and 'source'. 
-- 
-- At this time, 'facts' and 'source' are simply stubs to be fleshed out in 
-- later versions of 'Hasklepias'. 
data Context = Context {
      getConcepts :: Concepts
    , getFacts    :: Maybe Facts
    , getSource   :: Maybe Source
} deriving (Eq, Show)

-- | Defines composability of @Context@s.
instance Semigroup Context where
    x <> y = context (unBox $ Box (getConcepts x) <> Box (getConcepts y))

-- | Smart contructor for Context type
--
-- At this time, the 'facts' and 'source' are both 'Nothing'.
context :: [Concept] -> Context
context x = Context x Nothing Nothing

-- | Just an empty Context
emptyContext :: Context
emptyContext = Context [] Nothing Nothing

{-
The 'HasConcept' typeclass provides predicate functions for determining whether
an 'a' has a concept.
-}
class HasConcept a where
    -- | Does an 'a' have a particular 'Concept'?
    hasConcept  :: a -> Concept -> Bool

    -- | Does an 'a' have any of these 'Concepts'?
    hasConcepts :: a -> Concepts -> Bool
    hasConcepts x = any (x `hasConcept`)

instance HasConcept Context where
    hasConcept ctxt concept  = concept `elem` getConcepts ctxt

type Concept = Text
type Concepts = [Text]

newtype Box a = Box { unBox :: a } deriving (Eq, Show)
instance Semigroup (Box Concepts) where
    Box x <> Box y = Box $ union x y

data Facts  = Facts  deriving (Eq, Show)
data Source = Source deriving (Eq, Show)
