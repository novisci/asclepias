{-|
Module      : Hasklepias Event accessors
Description : Methods fro accessing data in events.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EventData.Accessors
  ( viewBirthYears
  , viewGenders
  , viewStates
  , previewDemoInfo
  , previewBirthYear
  ) where

import           Control.Lens                   ( (^.)
                                                , preview
                                                )
import           Control.Monad                  ( (=<<)
                                                , Functor(fmap)
                                                )
import           Data.Either                    ( either )
import           Data.Foldable                  ( toList )
import           Data.Function                  ( ($)
                                                , (.)
                                                , const
                                                )
import           Data.Functor.Contravariant     ( Predicate(..) )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Ord                       ( Ord )
import           Data.Text                      ( Text )
import           Data.Text.Read                 ( rational )
import           Data.Time.Calendar             ( Day
                                                , DayOfMonth
                                                , MonthOfYear
                                                , Year
                                                , diffDays
                                                , toGregorian
                                                )
import           Data.Tuple                     ( fst )
import           EventData.Context              ( Concepts
                                                , Context(..)
                                                , Source
                                                , facts
                                                , hasConcepts
                                                )
import           EventData.Context.Domain       ( Domain
                                                , _Demographics
                                                , demo
                                                , info
                                                )
import           EventData.Core                 ( Event
                                                , ctxt
                                                )
import           EventData.Predicates           ( isBirthYearEvent
                                                , isGenderFactEvent
                                                , isStateFactEvent
                                                )
import           GHC.Num                        ( Integer
                                                , fromInteger
                                                )
import           GHC.Real                       ( RealFrac(floor) )
import           Witherable                     ( Filterable(filter, mapMaybe)
                                                , Witherable
                                                )

-- | Preview demographics information from a domain
previewDemoInfo :: Domain -> Maybe Text
previewDemoInfo dmn = (^. demo . info) =<< preview _Demographics dmn

-- | Utility for reading text into a maybe integer
intMayMap :: Text -> Maybe Integer -- TODO: this is ridiculous
intMayMap x =
  fmap floor (either (const Nothing) (Just . fst) (Data.Text.Read.rational x))

-- | Preview birth year from a domain
previewBirthYear :: Domain -> Maybe Year
previewBirthYear dmn = intMayMap =<< previewDemoInfo dmn

-- | Returns a (possibly empty) list of birth years from a set of events
viewBirthYears :: (Witherable f) => f (Event a) -> [Year]
viewBirthYears x = mapMaybe
  (\e -> previewBirthYear =<< Just (ctxt e ^. facts))
  (toList $ filter (getPredicate isBirthYearEvent) x)

-- | Returns a (possibly empty) list of Gender values from a set of events
viewGenders :: (Witherable f) => f (Event a) -> [Text]
viewGenders x = mapMaybe (\e -> previewDemoInfo =<< Just (ctxt e ^. facts))
                         (toList $ filter (getPredicate isGenderFactEvent) x)

-- | Returns a (possibly empty) list of Gender values from a set of events
viewStates :: (Witherable f) => f (Event a) -> [Text]
viewStates x = mapMaybe (\e -> previewDemoInfo =<< Just (ctxt e ^. facts))
                        (toList $ filter (getPredicate isStateFactEvent) x)
