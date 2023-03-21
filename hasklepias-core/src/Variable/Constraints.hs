-- | Set of constraints all types wrapped in Variable must abide by.

{-# LANGUAGE ConstraintKinds #-}

module Variable.Constraints where

import           Data.Aeson      (ToJSON)
import           Type.Reflection (Typeable)

type VariableConstraints a = (ToJSON a, Typeable a, Show a)
