{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}


module Planning.Input where

import           Data.Text                      ( Text )
import           Data.Void
import qualified Dhall
import           Dhall.Core                     ( Expr )
import           Dhall.Src                      ( Src )
import           Map.Internal                   ( Map(..)
                                                , TestAtomic(..)
                                                , decodeMap
                                                , TestMap
                                                , Atomizable
                                                )
import           Planning.Decode.Dhall
import           Witch.From
import           Witch.TryFrom
import           Witch.TryFromException

  {- Flat test record type 

      THIS MODULE HAS UNCLEAR USEFULNESS AND SHOULD BE VIEWED AS AN EXPERIMENT
      SEE EVENT MODULE AND EXAMPLES
      Input type is one that can be converted to and from TestAtomic

      NOTE:
      * You might wonder why we can't replicate this pattern for FromDhall,
      adding an FromDhall constraint to InputVal. The issue appears to be that
      FromDhall instance methods operate on *types* not values, so we can't
      write a instance FromDhall InputVal because InputVal because we need
      access to the type. See the ambiguous type error here.  I do not fully
      understand this, however, so there might be a workaround.

data Dhallable = forall v. (Dhall.FromDhall v) => MkDhallable v

instance Dhall.FromDhall Dhallable where
  autoWith _ = MkDhallable <$> Dhall.auto
      -}

-- Input is a Map with any value that can be converted from TestAtomic,
-- possibly with failure
-- See note on awful TryFrom TestAtomic v requirement in Map.Internal
data InputVal = forall v. (Show v, Atomizable v) => MkInputVal v
type Input = Map InputVal

deriving instance Show InputVal

instance From InputVal TestAtomic where
  from (MkInputVal x) = from x

instance TryFrom TestAtomic InputVal where
  tryFrom = fmap MkInputVal . tryFrom

instance TryFrom TestMap Input where
  tryFrom m = convert out
   where
    convert (Left (TryFromException _ _)) =
      Left (TryFromException m Nothing :: (TryFromException TestMap Input))
    convert (Right x) = Right (Map x)
    -- TODO why doesn't traversable work properly without destructuring?
    (Map mm) = m
    out = traverse (tryFrom @TestAtomic) mm
