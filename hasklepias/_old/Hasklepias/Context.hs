module Hasklepias.Context(
    Context(..)
  , Source
  , context
) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as M

-- | TODO: define Source and its purpose

type Source = Maybe (M.Map String String)

-- | TODO: define Context and its purpose
-- info answers "What is the meaning of this data?"
-- source answers "What is the origin of this data?"
-- TODO: define a contextualizable typeclass that defines the behavior
-- of Context instances

data Context a = Context {
      getConcepts :: Maybe [String] 
    , getFacts    :: Maybe a 
    , getSource   :: Source }
    deriving (Eq, Show) 

-- | Smart contructor for Context type

context :: Maybe [String] -> Maybe a -> Source -> Context a
context = Context


