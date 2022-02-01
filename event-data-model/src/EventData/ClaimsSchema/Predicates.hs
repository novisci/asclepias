{-|
Module      : Hasklepias Event Type
Description : Defines the Event type and its component types, constructors, 
              and class instance
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}


module EventData.ClaimsSchema.Predicates
  ( isEnrollmentEvent
  , isStateFactEvent
  , isGenderFactEvent
  , isBirthYearEvent
  , isRegionFactEvent
  , isEligibilityEvent
  , containsConcepts
  ) where

import           Data.Bool                      ( Bool(..) )
import           Data.Function                  ( (.) )
import           Data.Maybe                     ( Maybe )
import           Data.Ord                       ( Ord )
import           Data.Text                      ( Text )
import           EventData.ClaimsSchema         ( ClaimsSchema(..) )
import           EventData.Domain.Demographics
import           EventDataTheory

-- | Predicate for Enrollment facts
isEnrollmentDomain :: ClaimsSchema -> Bool
isEnrollmentDomain (Enrollment _) = True
isEnrollmentDomain _              = False

-- | Predicate for enrollment events
isEnrollmentEvent :: Predicate (Event ClaimsSchema c a)
isEnrollmentEvent = liftToEventPredicate (Predicate isEnrollmentDomain)

-- | Predicate for Eligibility facts
isEligibilityDomain :: ClaimsSchema -> Bool
isEligibilityDomain (Eligibility _) = True
isEligibilityDomain _               = False

-- | Predicate for Eligibility events
isEligibilityEvent :: Predicate (Event ClaimsSchema c a)
isEligibilityEvent = liftToEventPredicate (Predicate isEligibilityDomain)

-- | Predicate for Birth Year facts
isBirthYear :: ClaimsSchema -> Bool
isBirthYear (Demographics (DemographicsFacts (DemographicsInfo BirthYear _))) =
  True
isBirthYear _ = False

-- | Predicate for events containing Birth Year facts
isBirthYearEvent :: Predicate (Event ClaimsSchema c a)
isBirthYearEvent = liftToEventPredicate (Predicate isBirthYear)

-- | Predicate for Gender facts
isGenderFact :: ClaimsSchema -> Bool
isGenderFact (Demographics (DemographicsFacts (DemographicsInfo Gender _))) =
  True
isGenderFact _ = False

-- | Predicate for events containing Gender facts
isGenderFactEvent :: Predicate (Event ClaimsSchema c a)
isGenderFactEvent = liftToEventPredicate (Predicate isGenderFact)

-- | Predicate for State facts
isStateFact :: ClaimsSchema -> Bool
isStateFact (Demographics (DemographicsFacts (DemographicsInfo State _))) =
  True
isStateFact _ = False

-- | Predicate for events containing  State facts
isStateFactEvent :: Predicate (Event ClaimsSchema c a)
isStateFactEvent = liftToEventPredicate (Predicate isStateFact)

-- | Predicate for Region facts
isRegionFact :: ClaimsSchema -> Bool
isRegionFact (Demographics (DemographicsFacts (DemographicsInfo Region _))) =
  True
isRegionFact _ = False

-- | Predicate for events containing Region facts
isRegionFactEvent :: Predicate (Event ClaimsSchema c a)
isRegionFactEvent = liftToEventPredicate (Predicate isRegionFact)
