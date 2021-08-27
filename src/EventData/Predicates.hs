{-|
Module      : Hasklepias Event Type
Description : Defines the Event type and its component types, constructors, 
              and class instance
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE Safe #-}

module EventData.Predicates
  ( isEnrollmentEvent
  , isStateFactEvent
  , isGenderFactEvent
  , isBirthYearEvent
  , containsConcepts
  , Predicatable(..)
  ) where

import           Data.Bool                      ( (&&)
                                                , Bool(..)
                                                , (||)
                                                )
import           Data.Function                  ( (.) )
import           Data.Functor.Contravariant     ( Contravariant(contramap)
                                                , Predicate(..)
                                                )
import           Data.Maybe                     ( Maybe )
import           Data.Ord                       ( Ord )
import           Data.Text                      ( Text )
import           EventData.Context              ( Concepts
                                                , Context(..)
                                                , Source
                                                , hasConcepts
                                                )
import           EventData.Context.Domain       ( Domain(..) )
import           EventData.Context.Domain.Demographics
import           EventData.Core                 ( Event
                                                , ctxt
                                                )
import           IntervalAlgebra                ( Interval
                                                , Intervallic(getInterval)
                                                )

{- |
  Provides methods for composing predicate functions (i.e. @a -> Bool@) or 
  'Predicate's by conjunction or disjunction.
-}
class Predicatable a where
  (|||) :: a -> a -> a
  (&&&) :: a -> a -> a

instance Predicatable (a -> Bool) where
  (|||) f g = \x -> f x || g x
  (&&&) f g = \x -> f x && g x

instance Predicatable (Predicate a) where
  (|||) p1 p2 = Predicate (getPredicate p1 ||| getPredicate p2)
  (&&&) p1 p2 = Predicate (getPredicate p1 &&& getPredicate p2)

{- |
  Provides a common interface to lift a 'Predicate' on a component of an 'Event'
  to a 'Predicate (Event a)'.
-}
class EventPredicate element a where
  liftToEventPredicate :: Predicate element -> Predicate (Event a)

instance EventPredicate Context a where
  liftToEventPredicate = contramap ctxt

instance EventPredicate Domain a where
  liftToEventPredicate = contramap (_facts . ctxt)

instance EventPredicate Concepts a where
  liftToEventPredicate = contramap (_concepts . ctxt)

instance EventPredicate (Maybe Source) a where
  liftToEventPredicate = contramap (_source . ctxt)

instance (Ord a) => EventPredicate (Interval a) a where
  liftToEventPredicate = contramap getInterval

{----------- Predicates  -----------------}

-- | Predicate for State facts
isEnrollmentDomain :: Domain -> Bool
isEnrollmentDomain (Enrollment _) = True
isEnrollmentDomain _              = False

-- | Predicate for enrollment events
isEnrollmentEvent :: Predicate (Event a)
isEnrollmentEvent = liftToEventPredicate (Predicate isEnrollmentDomain)

-- | Predicate for Birth Year facts
isBirthYear :: Domain -> Bool
isBirthYear (Demographics (DemographicsFacts (DemographicsInfo BirthYear _))) =
  True
isBirthYear _ = False

-- | Predicate for events containing Birth Year facts
isBirthYearEvent :: Predicate (Event a)
isBirthYearEvent = liftToEventPredicate (Predicate isBirthYear)

-- | Predicate for Gender facts
isGenderFact :: Domain -> Bool
isGenderFact (Demographics (DemographicsFacts (DemographicsInfo Gender _))) =
  True
isGenderFact _ = False

-- | Predicate for events containing Gender facts
isGenderFactEvent :: Predicate (Event a)
isGenderFactEvent = liftToEventPredicate (Predicate isGenderFact)

-- | Predicate for State facts
isStateFact :: Domain -> Bool
isStateFact (Demographics (DemographicsFacts (DemographicsInfo State _))) =
  True
isStateFact _ = False

-- | Predicate for events containing  State facts
isStateFactEvent :: Predicate (Event a)
isStateFactEvent = liftToEventPredicate (Predicate isStateFact)

<<<<<<< HEAD:src/EventData/Predicates.hs
-- | Creates a predicate to check that an 'Event' contains a set of 'EventData.Context.Concept's. 
=======
-- | Creates a predicate to check that an 'Event' contains a set of 'Concept's. 
>>>>>>> master:src/EventData/Predicate.hs
containsConcepts :: [Text] -> Predicate (Event a)
containsConcepts cpt = Predicate (`hasConcepts` cpt)
