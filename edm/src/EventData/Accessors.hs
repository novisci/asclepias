{-|
Module      : Hasklepias Event accessors
Description : Methods fro accessing data in events.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module EventData.Accessors
  ( viewBirthYears
  , viewGenders
  , viewStates
  , viewRegions
  , viewBenefits
  , previewCode
  , previewCodeE
  , previewBenefit
  , previewBenefitE
  , previewExchange
  , previewExchangeE
  , previewDemoInfo
  , previewBirthYear
  , previewDaysSupply
  , previewPlan
  , previewProvider
  , previewDischargeStatus
  , previewLengthOfStay
  ) where

import           Lens.Micro                     ( (^?) )
import           Control.Applicative            ( Alternative((<|>)), (<*>) )
import           Control.Monad                  ( (=<<)
                                                , (>>=)
                                                , Functor(fmap)
                                                )
import           Data.Either                    ( either )
import           Data.Foldable                  ( toList )
import           Data.Function                  ( ($)
                                                , (.)
                                                , const
                                                )
import           Data.Functor                   ((<&>))
import           Data.Functor.Contravariant     ( Predicate(..) )
import           Data.Generics.Internal.VL.Lens ( (^.) )
import           Data.Generics.Product          ( HasField(field) )
import           Data.Generics.Sum              ( AsAny(_As) )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Ord                       ( Ord )
import           Data.Semigroup                 ( (<>) )
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
import           EventData.Context.Domain       ( Domain(..)
                                                , demo
                                                , info
                                                )
import           EventData.Context.Facts
import           EventData.Core                 ( Event
                                                , ctxt
                                                )
import           EventData.Predicates           ( isBirthYearEvent
                                                , isGenderFactEvent
                                                , isStateFactEvent
                                                , isRegionFactEvent
                                                , isEnrollmentEvent
                                                , isEligibilityEvent
                                                )
import           GHC.Float                      ( Double )
import           GHC.Num                        ( Integer
                                                , fromInteger
                                                )
import           GHC.Real                       ( RealFrac(floor) )
import           Witherable                     ( Filterable(filter, mapMaybe)
                                                , Witherable
                                                )

-- | Preview demographics information from a domain
previewDemoInfo :: Domain -> Maybe Text
previewDemoInfo dmn =
  (^. field @"demo" . field @"info") =<< (dmn ^? _As @"Demographics")

-- | Utility for reading text into a maybe integer
intMayMap :: Text -> Maybe Integer -- TODO: this is ridiculous
intMayMap x =
  fmap floor (either (const Nothing) (Just . fst) (Data.Text.Read.rational x))

-- | Preview days supply field information from a medication domain
previewDaysSupply :: Domain -> Maybe Integer 
previewDaysSupply dmn =
  (dmn ^? _As @"Medication") >>= (^. field @"fill") >>= (^. field @"days_supply")

-- | Preview the text part of a 'Code' from a 'Diagnosis', 'Labs', 
--  'Medication', or 'Procedure' Domain.
previewCode :: Domain -> Maybe Text 
previewCode dmn =
      ((dmn ^? (_As @"Diagnosis")) <&> (^. field @"code" . field @"code" ))
  <|> ((dmn ^? (_As @"Labs")) <&> (^. field @"code" . field @"code"))
  <|> ((dmn ^? (_As @"Medication")) <&> (^. field @"code" . field @"code"))
  <|> ((dmn ^? (_As @"Procedure")) <&> (^. field @"code" . field @"code"))

-- | Preview the text part of a 'Code' from an event, using `previewCode'.
previewCodeE :: Event a -> Maybe Text
previewCodeE = previewCode . facts . ctxt 

-- | Preview @Provider@ from 'Diagnosis', 'Medication', or 'Procedure' Domain
previewProvider :: Domain -> Maybe Provider 
previewProvider dmn =
      ((dmn ^? (_As @"Diagnosis")) >>= (^. field @"provider"))
  <|> ((dmn ^? (_As @"Medication")) >>= (^. field @"provider"))
  <|> ((dmn ^? (_As @"Procedure")) >>= (^. field @"provider"))

-- | Preview @Plan@ from 'Eligibility' or 'Enrollment' Domain
previewPlan :: Domain -> Maybe Plan 
previewPlan dmn =
      ((dmn ^? (_As @"Eligibility")) >>= (^. field @"plan"))
  <|> ((dmn ^? (_As @"Enrollment")) >>= (^. field @"plan"))

-- | Preview @discharge_status@ f rom the 'Hospitalization' fact of'Diagnosis' or 'Procedure' Domain
previewDischargeStatus :: Domain -> Maybe Text 
previewDischargeStatus dmn =
      ((dmn ^? (_As @"Diagnosis")) >>= (^. field @"hospitalization") >>= (^. field @"discharge_status"))
  <|> ((dmn ^? (_As @"Procedure")) >>= (^. field @"hospitalization") >>= (^. field @"discharge_status"))

-- | Preview @length_of_stay@ from the 'Hospitalization' fact of 'Diagnosis' or 'Procedure' Domain
previewLengthOfStay :: Domain -> Maybe Double 
previewLengthOfStay dmn =
      ((dmn ^? (_As @"Diagnosis")) >>= (^. field @"hospitalization") >>= (^. field @"length_of_stay"))
  <|> ((dmn ^? (_As @"Procedure")) >>= (^. field @"hospitalization") >>= (^. field @"length_of_stay"))


-- | View the @benefit@ field of a @Plan@
previewBenefit :: Domain -> Maybe [Text]
previewBenefit x = previewPlan x >>= (^? field @"benefit") >>= fmap singleOrArrayToList

-- | View the @exchange@ field of a @Event@
previewBenefitE :: Event a -> Maybe [Text]  
previewBenefitE = previewBenefit . facts . ctxt

-- | View the @exchange@ field of a @Plan@
previewExchange :: Domain -> Maybe Exchange 
previewExchange x = previewPlan x >>= (^? field @"exchange")

-- | View the @exchange@ field of a @Event@
previewExchangeE :: Event a -> Maybe Exchange  
previewExchangeE = previewExchange . facts . ctxt 

-- | Preview birth year from a domain
previewBirthYear :: Domain -> Maybe Year
previewBirthYear dmn = intMayMap =<< previewDemoInfo dmn

-- | Returns a (possibly empty) list of birth years from a set of events
viewBirthYears :: (Witherable f) => f (Event a) -> [Year]
viewBirthYears x = mapMaybe
  (\e -> previewBirthYear (facts $ ctxt e))
  (toList $ filter (getPredicate isBirthYearEvent) x)

-- | Returns a (possibly empty) list of Gender values from a set of events
viewGenders :: (Witherable f) => f (Event a) -> [Text]
viewGenders x = mapMaybe
  (\e -> previewDemoInfo =<< Just (ctxt e ^. field @"facts"))
  (toList $ filter (getPredicate isGenderFactEvent) x)

-- | Returns a (possibly empty) list of State values from a set of events
viewStates :: (Witherable f) => f (Event a) -> [Text]
viewStates x = mapMaybe
  (\e -> previewDemoInfo =<< Just (ctxt e ^. field @"facts"))
  (toList $ filter (getPredicate isStateFactEvent) x)

-- | Returns a (possibly empty) list of Region values from a set of events
viewRegions :: (Witherable f) => f (Event a) -> [Text]
viewRegions x = mapMaybe
  (\e -> previewDemoInfo =<< Just (ctxt e ^. field @"facts"))
  (toList $ filter (getPredicate isRegionFactEvent) x)

-- | Returns a (possibly empty) list of Insurance plan values from a set of events
viewBenefits :: (Witherable f) => f (Event a) -> [Text]
viewBenefits x = mapMaybe
  (\e -> previewBenefit =<< Just (ctxt e ^. field @"facts"))
  (toList $ filter (getPredicate isEnrollmentEvent ||| isEligibilityEvent) x) |> concat

