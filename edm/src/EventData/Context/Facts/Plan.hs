{-|
Module      : Plan fact
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Facts.Plan(
    Plan(..)
  , emptyPlan
) where
import Data.Aeson               ( FromJSON )
import Data.Eq                  ( Eq )
import Data.Maybe               ( Maybe(..) )
import Data.Text                ( Text )
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )

import EventData.Context.Facts.Exchange ( Exchange(..) )

-- | plan
data Plan = Plan {
    exchange :: Exchange
  , plan_id  :: Maybe Text
  , group_id :: Maybe Text
  , subscriber_id :: Maybe Text
  , subscriber_relationship :: Maybe Text
  , benefit :: Maybe Text -- TODO: Maybe is different from EDM 1.1 spec
  }
   deriving (Eq, Show, Generic)

-- | utility to create an empty plan
emptyPlan :: Plan
emptyPlan = Plan None Nothing Nothing Nothing Nothing Nothing

instance FromJSON Plan where