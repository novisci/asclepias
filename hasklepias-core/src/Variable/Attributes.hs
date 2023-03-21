-- | Internal module defining top-level attributes needed for a 'Variable'.

{-# LANGUAGE DeriveGeneric #-}
module Variable.Attributes where

import           Data.Aeson   (ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | Internal. Variable-level attributes. This will not be constructed directly
-- but instead as part of the smart constructors of 'Variable'. Only 'varName'
-- is user-supplied, and 'varType' is automatically produced. This is only the
-- minimal layer of attributes needed, and more can and will be produced to
-- construct 'VariableWrapped'.
data VarAttrs
  = MkVarAttrs
      { varType :: Text
      , varName :: Text
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON VarAttrs

