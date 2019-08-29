module Hasklepias.Context(
    Context
  , context
  , contextns
  , getDomain
  , getInfo
  , getSource
) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as M

-- | TODO: define Source and its purpose

type Source = Maybe (M.Map String String)

-- | TODO: define Context and its purpose

data Context a = Context {
      getDomain :: String
    , getInfo   :: a 
    , getSource :: Source}
    deriving (Eq, Show) 

-- | Smart contructor for Context type

context :: String -> a -> Source -> Context a
context d i s = Context d i s

-- | Smart contructor for Context type without a Source

contextns :: String -> a -> Context a
contextns d i = context d i Nothing