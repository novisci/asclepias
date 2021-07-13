{-|
Module      : Event Data Model Contexts
Description : Defines the Context type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE Safe #-}
{-# LANGUAGE TemplateHaskell #-}

module EventData.Context(
    Context(..)
  , concepts
  , facts
  , source
  , context

  , Concept
  , Concepts
  , toConcepts
  , getConcepts
  , packConcept
  , unpackConcept
  , packConcepts
  , unpackConcepts
  , HasConcept(..)
) where

import Control.Lens             ( makeLenses )
import GHC.Show                 ( Show(show) )
import Data.Bool                ( Bool )
import Data.Eq                  ( Eq )
import Data.Function            ( (.), ($) )
import Data.List                ( all, any, map )
import Data.Maybe               ( Maybe(Nothing) )
import Data.Monoid              ( (<>), Monoid(mempty) )
import Data.Ord                 ( Ord )
import Data.Semigroup           ( Semigroup((<>)) )
import Data.Text                ( Text )
import Data.Set                 ( Set
                                , fromList, union, empty, map, toList, member)
import EventData.Context.Domain ( Domain )

-- | A @Context@ consists of three parts: @concepts@, @facts@, and @source@. 
-- 
-- At this time, @facts@ and @source@ are simply stubs to be fleshed out in 
-- later versions of hasklepias. 
data Context = Context {
      _concepts :: Concepts
    , _facts    :: Domain
    , _source   :: Maybe Source
} deriving (Eq, Show)

data Source = Source deriving (Eq, Show)

instance HasConcept Context where
    hasConcept ctxt concept = 
        member (packConcept concept) (getConcepts $ _concepts ctxt)

-- | Smart contructor for Context type
--
-- Creates 'Context' from a list of 'Concept's. At this time, the @facts@ and
-- @source@ are both set to 'Nothing'.
context :: Domain -> Concepts -> Context
context d x = Context x d Nothing

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

-- | @Concepts@ is a 'Set' of 'Concept's.
newtype Concepts = Concepts { getConcepts :: Set Concept }
    deriving (Eq, Show)

instance Semigroup Concepts where
    Concepts x <> Concepts y = Concepts (x <> y)

instance Monoid Concepts where
    mempty = Concepts mempty

-- | Constructor for 'Concepts'.
toConcepts :: Set Concept -> Concepts
toConcepts = Concepts

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

makeLenses ''Context