{-|
Module      : Hasklepias Contexts
Description : Defines the Context type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe #-}

module Hasklepias.Types.Context(
    Context(getConcepts)
  , context
  , emptyContext

  , Concept
  , Concepts
  , toConcepts
  , fromConcepts
  , packConcept
  , unpackConcept
  , packConcepts
  , unpackConcepts
  , HasConcept(..)
) where

import GHC.Show                 ( Show(show) )
import Data.Bool                ( Bool )
import Data.Eq                  ( Eq )
import Data.Function            ((.), ($))
import Data.List                ( all, any, map )
import Data.Maybe               ( Maybe(Nothing) )
import Data.Monoid              ( (<>), Monoid(mempty) )
import Data.Ord                 ( Ord )
import Data.Semigroup           ( Semigroup((<>)) )
import Data.Text                (Text)
import Data.Set                 (Set
                                , fromList, union, empty, map, toList, member)

-- | A @Context@ consists of three parts: @concepts@, @facts@, and @source@. 
-- 
-- At this time, @facts@ and @source@ are simply stubs to be fleshed out in 
-- later versions of hasklepias. 
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
    hasConcept ctxt concept = 
        member (packConcept concept) (fromConcepts $ getConcepts ctxt)

-- | Smart contructor for Context type
--
-- Creates 'Context' from a list of 'Concept's. At this time, the @facts@ and
-- @source@ are both set to 'Nothing'.
context :: Concepts -> Context
context x = Context x Nothing Nothing

-- | Just an empty Context
emptyContext :: Context
emptyContext = Context mempty Nothing Nothing

-- | A @Concept@ is textual "tag" for a context.
newtype Concept = Concept Text deriving (Eq, Ord)

instance Show Concept where
    show (Concept x) = show x

-- | Pack text into a concept
packConcept :: Text -> Concept
packConcept = Concept

-- | Unpack text from a concept
unpackConcept :: Concept -> Text 
unpackConcept (Concept x) =  x

-- | @Concepts@ is a 'Set' of 'Concepts's.
newtype Concepts = Concepts ( Set Concept )
    deriving (Eq, Show)

instance Semigroup Concepts where
    Concepts x <> Concepts y = Concepts (x <> y)

instance Monoid Concepts where
    mempty = Concepts mempty

-- | Constructor for 'Concepts'.
toConcepts :: Set Concept -> Concepts
toConcepts = Concepts

fromConcepts :: Concepts -> Set Concept
fromConcepts (Concepts x) = x

-- | Put a list of text into a set concepts.
packConcepts :: [Text] -> Concepts
packConcepts x = Concepts $ fromList $ Data.List.map packConcept x

-- | Take a set of concepts to a list of text.
unpackConcepts :: Concepts -> [Text]
unpackConcepts (Concepts x) = toList $ Data.Set.map unpackConcept x 

{- |
The 'HasConcept' typeclass provides predicate functions for determining whether
an @a@ has a concept.
-}
class HasConcept a where
    -- | Does an @a@ have a particular 'Concept'?
    hasConcept  :: a -> Text -> Bool

    -- | Does an @a@ have *any* of a list of 'Concept's?
    hasConcepts :: a -> [Text] -> Bool
    hasConcepts x = any (\c -> x `hasConcept` c)

    -- | Does an @a@ have *all* of a list of `Concept's?
    hasAllConcepts :: a -> [Text] -> Bool
    hasAllConcepts x = all (\c -> x `hasConcept` c) 

instance HasConcept Concepts where
    hasConcept (Concepts e) concept = member (packConcept concept) e