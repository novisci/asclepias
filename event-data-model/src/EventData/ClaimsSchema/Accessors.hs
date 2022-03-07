{-|
Module      : Hasklepias Event ClaimsSchema c accessors
Description : Methods fro accessing data in events.
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module EventData.ClaimsSchema.Accessors
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

import           Control.Applicative            ( (<*>)
                                                , Alternative((<|>))
                                                )
import           Control.Monad                  ( (=<<)
                                                , (>>=)
                                                , Functor(fmap)
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Foldable                  ( toList )
import           Data.Function                  ( ($)
                                                , (.)
                                                , const
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Generics.Internal.VL.Lens ( (^.) )
import           Data.Generics.Product          ( HasField(field) )
import           Data.Generics.Sum              ( AsAny(_As) )
import           Data.List                      ( concat )
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
import           EventData.ClaimsSchema         ( ClaimsSchema(..)
                                                , demo
                                                , info
                                                )
import           EventData.ClaimsSchema.Predicates
import           EventData.Facts
import           EventDataTheory                ( Event(..)
                                                , Predicate(..)
                                                , getContext
                                                , getFacts
                                                , (|||)
                                                )
import           GHC.Float                      ( Double )
import           GHC.Num                        ( Integer
                                                , fromInteger
                                                )
import           GHC.Real                       ( RealFrac(floor) )
import           Lens.Micro                     ( (^?) )
import           Witherable                     ( Filterable(filter, mapMaybe)
                                                , Witherable
                                                )

-- | Preview demographics information from a ClaimsSchema
previewDemoInfo :: ClaimsSchema -> Maybe Text
previewDemoInfo dmn =
  (^. field @"demo" . field @"info") =<< (dmn ^? _As @"Demographics")

-- | Utility for reading text into a maybe integer
intMayMap :: Text -> Maybe Integer -- TODO: this is ridiculous
intMayMap x = fmap
  floor
  ((\case
     Left  _ -> Nothing
     Right v -> Just (fst v)
   )
    (Data.Text.Read.rational x)
  )

-- | Preview days supply field information from a medication ClaimsSchema
previewDaysSupply :: ClaimsSchema -> Maybe Integer
previewDaysSupply dmn =
  (dmn ^? _As @"Medication")
    >>= (^. field @"fill")
    >>= (^. field @"days_supply")

-- | Preview the text part of a 'Code' from a 'Diagnosis', 'Labs', 
--  'Medication', or 'Procedure' ClaimsSchema.
previewCode :: ClaimsSchema -> Maybe Text
previewCode dmn =
  ((dmn ^? (_As @"Diagnosis")) <&> (^. field @"code" . field @"code"))
    <|> ((dmn ^? (_As @"Labs")) <&> (^. field @"code" . field @"code"))
    <|> ((dmn ^? (_As @"Medication")) <&> (^. field @"code" . field @"code"))
    <|> ((dmn ^? (_As @"Procedure")) <&> (^. field @"code" . field @"code"))

-- | Preview the text part of a 'Code' from an event, using `previewCode'.
previewCodeE :: Event c ClaimsSchema a -> Maybe Text
previewCodeE = previewCode . getFacts . getContext

-- | Preview @Provider@ from 'Diagnosis', 'Medication', or 'Procedure' ClaimsSchema
previewProvider :: ClaimsSchema -> Maybe Provider
previewProvider dmn =
  ((dmn ^? (_As @"Diagnosis")) >>= (^. field @"provider"))
    <|> ((dmn ^? (_As @"Medication")) >>= (^. field @"provider"))
    <|> ((dmn ^? (_As @"Procedure")) >>= (^. field @"provider"))

-- | Preview @Plan@ from 'Eligibility' or 'Enrollment' ClaimsSchema
previewPlan :: ClaimsSchema -> Maybe Plan
previewPlan dmn =
  ((dmn ^? (_As @"Eligibility")) >>= (^. field @"plan"))
    <|> ((dmn ^? (_As @"Enrollment")) >>= (^. field @"plan"))

-- | Preview @discharge_status@ f rom the 'Hospitalization' fact of'Diagnosis' or 'Procedure' ClaimsSchema
previewDischargeStatus :: ClaimsSchema -> Maybe Text
previewDischargeStatus dmn =
  (   (dmn ^? (_As @"Diagnosis"))
    >>= (^. field @"hospitalization")
    >>= (^. field @"discharge_status")
    )
    <|> (   (dmn ^? (_As @"Procedure"))
        >>= (^. field @"hospitalization")
        >>= (^. field @"discharge_status")
        )

-- | Preview @length_of_stay@ from the 'Hospitalization' fact of 'Diagnosis' or 'Procedure' ClaimsSchema
previewLengthOfStay :: ClaimsSchema -> Maybe Double
previewLengthOfStay dmn =
  (   (dmn ^? (_As @"Diagnosis"))
    >>= (^. field @"hospitalization")
    >>= (^. field @"length_of_stay")
    )
    <|> (   (dmn ^? (_As @"Procedure"))
        >>= (^. field @"hospitalization")
        >>= (^. field @"length_of_stay")
        )


-- | View the @benefit@ field of a @Plan@
previewBenefit :: ClaimsSchema -> Maybe Text
previewBenefit x = previewPlan x >>= (^. field @"benefit")

-- | View the @benefit@ field of a @Event@
previewBenefitE :: Event c ClaimsSchema a -> Maybe Text
previewBenefitE = previewBenefit . getFacts . getContext

-- | View the @exchange@ field of a @Plan@
previewExchange :: ClaimsSchema -> Maybe Exchange
previewExchange x = previewPlan x >>= (^? field @"exchange")

-- | View the @exchange@ field of a @Event@
previewExchangeE :: Event c ClaimsSchema a -> Maybe Exchange
previewExchangeE = previewExchange . getFacts . getContext

-- | Preview birth year from a ClaimsSchema
previewBirthYear :: ClaimsSchema -> Maybe Year
previewBirthYear dmn = intMayMap =<< previewDemoInfo dmn

-- | Returns a (possibly empty) list of birth years from a set of events
viewBirthYears :: (Witherable f) => f (Event c ClaimsSchema a) -> [Year]
viewBirthYears x = mapMaybe
  (\e -> previewBirthYear (getFacts $ getContext e))
  (toList $ filter (getPredicate isBirthYearEvent) x)

-- | Returns a (possibly empty) list of Gender values from a set of events
viewGenders :: (Witherable f) => f (Event c ClaimsSchema a) -> [Text]
viewGenders x = mapMaybe
  (\e -> previewDemoInfo =<< Just (getContext e ^. field @"getFacts"))
  (toList $ filter (getPredicate isGenderFactEvent) x)

-- | Returns a (possibly empty) list of State values from a set of events
viewStates :: (Witherable f) => f (Event c ClaimsSchema a) -> [Text]
viewStates x = mapMaybe
  (\e -> previewDemoInfo =<< Just (getContext e ^. field @"getFacts"))
  (toList $ filter (getPredicate isStateFactEvent) x)

-- | Returns a (possibly empty) list of Region values from a set of events
viewRegions :: (Witherable f) => f (Event c ClaimsSchema a) -> [Text]
viewRegions x = mapMaybe
  (\e -> previewDemoInfo =<< Just (getContext e ^. field @"getFacts"))
  (toList $ filter (getPredicate isRegionFactEvent) x)

-- | Returns a (possibly empty) list of Insurance plan benefit values from a set of events
viewBenefits :: (Witherable f) => f (Event c ClaimsSchema a) -> [Text]
viewBenefits x = mapMaybe
  (\e -> previewBenefit =<< Just (getContext e ^. field @"getFacts"))
  (toList $ filter (getPredicate (isEnrollmentEvent ||| isEligibilityEvent)) x)

