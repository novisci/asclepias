module Hasklepias.Context(
    Context
  , context
  , getDomain
  , getInfo
) where



-- | TODO: define context and its purpose
--   key = Maybe value, 
--newtype Context a = Context { getContext :: (M.Map String (Maybe a)) }
-- deriving (Eq, Ord, Show)

data Context a = Context {
      getDomain :: String
    , getInfo   :: a }
    deriving (Eq, Show) 

context :: String -> a -> Context a
context d i = Context d i