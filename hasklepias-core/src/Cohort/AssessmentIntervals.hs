{-|
Module      : Cohort Index 
Description : Defines the Index and related types and functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cohort.AssessmentIntervals
  (
  {- |
The assessment intervals provided are:

* 'Baseline': an interval which either 'IntervalAlgebra.meets' or
  'IntervalAlgebra.precedes' index. Covariates are typically assessed during
  baseline intervals. A cohort's specification may include multiple baseline
  intervals, as different features may require different baseline intervals.
  For example, one feature may use a baseline interval of 365 days prior to
  index, while another uses a baseline interval of 90 days before index up
  to 30 days before index.
* `Followup`: an interval which is 'IntervalAlgebra.startedBy', 
  'IntervalAlgebra.metBy', or 'IntervalAlgebra.after' an 'Index'. Outcomes
  are typically assessed during followup intervals. Similar to 'Baseline',
    a cohort's specification may include multiple followup intervals, 
    as different features may require different followup intervals. 

In future versions, one subject may have multiple values for an 'Index'
corresponding to unique 'Cohort.Core.ObsUnit'. That is, there is a 1-to-1 map between 
index values and observational units, but there may be a 1-to-many map from 
subjects to indices.

While users are protected from forming invalid assessment intervals, they still
need to carefully consider how to filter events based on the assessment interval. 
Consider the following data:

@
               _      <- Index    (15, 16)
     ----------       <- Baseline (5, 15)
 ---                  <- A (1, 4)
  ---                 <- B (2, 5)
    ---               <- C (4, 7)
      ---             <- D (5, 8)
         ---          <- E (8, 11)
            ---       <- F (12, 15)
              ---     <- G (14, 17)
                 ___  <- H (17, 20)
|----|----|----|----|
0         10        20
@

We have index, baseline, and 8 events (A-H). If Baseline is our assessment interval,
then the events concuring (i.e. not disjoint) with Baseline are C-G.  While C-F
probably make sense to use in deriving some covariate, what about G? The event G
begins during baseline but ends after index. If you want, for example, to know
how many events started during baseline, then you’d want to include G in your
filter (using 'IntervalAlgebra.concur'). But if you wanted to know the durations 
of events enclosed by baseline, then you wouldn’t want to filter using concur 
and instead perhaps use 'IntervalAlgebra.enclosedBy'.


    -}
    BaselineInterval
  , Baseline(..)
  , FollowupInterval
  , Followup(..)
  , AssessmentInterval
  , makeBaselineFromIndex
  , makeBaselineBeforeIndex
  , makeBaselineFinishedByIndex
  , makeFollowupFromIndex
  , makeFollowupMeetingIndex
  , makeFollowupAfterIndex
  ) where

import           Cohort.Index                   ( Index )
import           Data.Eq                        ( Eq )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( Functor(fmap) )
import           Data.Ord                       ( (<=)
                                                , Ord
                                                )
import           IntervalAlgebra                ( Interval
                                                , IntervalSizeable(..)
                                                , Intervallic(..)
                                                , begin
                                                , beginerval
                                                , duration
                                                , end
                                                , enderval
                                                , extenterval
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Num((+)) )
import           GHC.Show                       ( Show )

{-| A type to contain baseline intervals. See the 'Baseline' typeclass for methods
to create values of this type.
-}
newtype BaselineInterval a = MkBaselineInterval (Interval a)
  deriving (Eq, Show, Generic)

instance Functor BaselineInterval where
  fmap f (MkBaselineInterval x) = MkBaselineInterval (fmap f x)

instance (Ord a) => Intervallic BaselineInterval a where
  getInterval (MkBaselineInterval x) = getInterval x
  setInterval (MkBaselineInterval x) y = MkBaselineInterval (setInterval x y)

{-| 
Provides functions for creating a 'BaselineInterval' from an 'Index'. The 
'baseline' function should satify:

[Meets]

  @'IntervalAlgebra.relate' ('baseline' d i) i = 'IntervalAlgebra.Meets'@

The 'baselineBefore' function should satisfy:

[Before]
  
  @'IntervalAlgebra.relate' ('baselineBefore' s d i) i = 'IntervalAlgebra.Before'@

The 'baselineFinishedBy' function should satisfy:

[FinishedBy]
  
  @'IntervalAlgebra.relate' ('baselineFinishedBy' s d i) i = 'IntervalAlgebra.FinishedBy'@

>>> import Cohort.Index
>>> import IntervalAlgebra
>>> x = makeIndex (beginerval 1 10)
>>> b =baseline 10 x
>>> b
>>> relate b x
MkBaselineInterval (0, 10)
Meets

>>> import Cohort.Index
>>> import IntervalAlgebra
>>> x = makeIndex (beginerval 1 10)
>>> b = baselineBefore 2 4 x
>>> b
>>> relate b x
MkBaselineInterval (4, 8)
Before
-}
class Intervallic i a => Baseline i a where
  -- | Creates a 'BaselineInterval' of the given duration that 'IntervalAlgebra.Meets'
  -- the 'Index' interval.
  baseline ::
    ( IntervalSizeable a b) =>
      b -- ^ duration of baseline
    -> Index i a -- ^ the 'Index' event
    -> BaselineInterval a
  baseline dur index = MkBaselineInterval (enderval dur (begin index))

  -- | Creates a 'BaselineInterval' of the given duration that 'IntervalAlgebra.precedes'
  -- the 'Index' interval. 
  baselineBefore ::
    ( IntervalSizeable a b) =>
       b -- ^ duration to shift back 
    -> b -- ^ duration of baseline
    -> Index i a -- ^ the 'Index' event
    -> BaselineInterval a
  baselineBefore shiftBy dur index =
    MkBaselineInterval $ enderval dur (begin (enderval shiftBy (begin index)))

  -- | Creates a 'BaselineInterval' of the given duration that 'IntervalAlgebra.FinishedBy'
  -- the 'Index' interval. 
  baselineFinishedBy ::
    ( IntervalSizeable a b ) =>
       b -- ^ duration of baseline - not including the duration of index
    -> Index i a -- ^ the 'Index' event
    -> BaselineInterval a
  baselineFinishedBy dur index =
    MkBaselineInterval (extenterval (enderval dur (begin index)) (getInterval index))

instance (Ord a) => Baseline Interval a

{-| A type to contain followup intervals. See the 'Followup' typeclass for methods
to create values of this type.
-}
newtype FollowupInterval a = MkFollowupInterval (Interval a)
  deriving (Eq, Show, Generic)

instance Functor FollowupInterval where
  fmap f (MkFollowupInterval x) = MkFollowupInterval (fmap f x)

instance (Ord a) => Intervallic FollowupInterval a where
  getInterval (MkFollowupInterval x) = getInterval x
  setInterval (MkFollowupInterval x) y = MkFollowupInterval (setInterval x y)

{-| 
Provides functions for creating a 'FollowupInterval' from an 'Index'. The 
'followup' function should satify:

[StartedBy]

  @'IntervalAlgebra.relate' ('followup' d i) i = 'IntervalAlgebra.StartedBy'@

The 'followupMetBy' function should satisfy:

[MetBy]
  
  @'IntervalAlgebra.relate' ('followupMetBy' d i) i = 'IntervalAlgebra.MetBy'@

The 'followupAfter' function should satisfy:

[After]

  @'IntervalAlgebra.relate' ('followupAfter' s d i) i = 'IntervalAlgebra.After'@

>>> import Cohort.Index
>>> import IntervalAlgebra
>>> x = makeIndex (beginerval 1 10)
>>> f = followup 10 x
>>> f
>>> relate f x
MkFollowupInterval (10, 20)
StartedBy

Note the consequence of providing a duration less than or equal to the duration 
of the index: a 'IntervalAlgebra.moment' is added to the duration, so that the 
end of the 'FollowupInterval' is greater than the end of the 'Index'.

>>> import Cohort.Index
>>> import IntervalAlgebra
>>> x = makeIndex (beginerval 1 10)
>>> f = followup 1 x
>>> f
>>> relate f x
MkFollowupInterval (10, 12)
StartedBy

>>> import Cohort.Index
>>> import IntervalAlgebra
>>> x = makeIndex (beginerval 1 10)
>>> f = followupMetBy 9 x
>>> f
>>> relate f x
MkFollowupInterval (11, 20)
MetBy

>>> import Cohort.Index
>>> import IntervalAlgebra
>>> x = makeIndex (beginerval 1 10)
>>> f = followupAfter 1 9 x
>>> f
>>> relate f x
MkFollowupInterval (12, 21)
After
-}
class Intervallic i a => Followup i a where
  followup ::
    ( IntervalSizeable a b
    , Intervallic i a) =>
      b -- ^ duration of followup
    -> Index i a -- ^ the 'Index' event
    -> FollowupInterval a
  followup dur index = MkFollowupInterval (beginerval d2 (begin index))
    where d2 = if dur <= duration index
                 then duration index + moment' index
                 else dur

  followupMetBy ::
    ( IntervalSizeable a b
    , Intervallic i a) =>
      b -- ^ duration of followup
    -> Index i a -- ^ the 'Index' event
    -> FollowupInterval a
  followupMetBy dur index = MkFollowupInterval (beginerval dur (end index))

  followupAfter ::
    ( IntervalSizeable a b
    , Intervallic i a) =>
       b -- ^ duration add between the end of index and begin of followup
    -> b -- ^ duration of followup
    -> Index i a -- ^ the 'Index' event
    -> FollowupInterval a
  followupAfter shiftBy dur index =
    MkFollowupInterval $ beginerval dur (end (beginerval shiftBy (end index)))

instance (Ord a) => Followup Interval a

-- | A data type that contains variants of intervals during which assessment
-- may occur.
data AssessmentInterval a =
      Bl (BaselineInterval a) -- ^ holds a 'BaselineInterval'
    | Fl (FollowupInterval a) -- ^ holds a 'FollowupInterval'
    deriving (Eq, Show, Generic)

instance (Ord a) => Intervallic AssessmentInterval a where
  getInterval (Bl x) = getInterval x
  getInterval (Fl x) = getInterval x

  setInterval (Bl x) y = Bl (setInterval x y)
  setInterval (Fl x) y = Fl (setInterval x y)

instance Functor AssessmentInterval where
  fmap f (Bl x) = Bl (fmap f x)
  fmap f (Fl x) = Fl (fmap f x)

-- | Creates an 'AssessmentInterval' using the 'baseline' function. 
-- 
-- >>> import Cohort.Index
-- >>> x = makeIndex $ beginerval 1 10
-- >>> makeBaselineFromIndex 10 x
-- Bl (MkBaselineInterval (0, 10))
--
makeBaselineFromIndex
  :: (Baseline i a, IntervalSizeable a b)
  => b
  -> Index i a
  -> AssessmentInterval a
makeBaselineFromIndex dur index = Bl (baseline dur index)

-- | Creates an 'AssessmentInterval' using the 'baselineBefore' function. 
-- 
-- >>> import Cohort.Index
-- >>> x = makeIndex $ beginerval 1 10
-- >>> makeBaselineBeforeIndex 2 10 x
-- Bl (MkBaselineInterval (-2, 8))
--
makeBaselineBeforeIndex
  :: (Baseline i a, IntervalSizeable a b)
  => b
  -> b
  -> Index i a
  -> AssessmentInterval a
makeBaselineBeforeIndex shiftBy dur index =
  Bl (baselineBefore shiftBy dur index)

-- | Creates an 'AssessmentInterval' using the 'baselineFinishedBy' function. 
-- 
-- >>> import Cohort.Index
-- >>> x = makeIndex $ beginerval 1 10
-- >>> makeBaselineFinishedByndex 10 x
-- Bl (MkBaselineInterval (0, 11))
--
makeBaselineFinishedByIndex
  :: (Baseline i a, IntervalSizeable a b)
  => b
  -> Index i a
  -> AssessmentInterval a
makeBaselineFinishedByIndex dur index = Bl (baselineFinishedBy dur index)

-- | Creates an 'AssessmentInterval' using the 'followup' function. 
-- 
-- >>> import Cohort.Index
-- >>> x = makeIndex $ beginerval 1 10
-- >>> makeFollowupFromIndex 10 x
-- Fl (MkFollowupInterval (10, 20))
--
makeFollowupFromIndex
  :: (Followup i a, IntervalSizeable a b)
  => b
  -> Index i a
  -> AssessmentInterval a
makeFollowupFromIndex dur index = Fl (followup dur index)

-- | Creates an 'AssessmentInterval' using the 'followupMetBy' function. 
-- 
-- >>> import Cohort.Index
-- >>> x = makeIndex $ beginerval 1 10
-- >>> makeFollowupMeetingIndex 10 x
-- Fl (MkFollowupInterval (11, 21))
--
makeFollowupMeetingIndex
  :: (Followup i a, IntervalSizeable a b)
  => b
  -> Index i a
  -> AssessmentInterval a
makeFollowupMeetingIndex dur index = Fl (followupMetBy dur index)

-- | Creates an 'AssessmentInterval' using the 'followupAfter' function. 
-- 
-- >>> import Cohort.Index
-- >>> x = makeIndex $ beginerval 1 10
-- >>> makeFollowupAfterIndex 10 10 x
-- Fl (MkFollowupInterval (21, 31))
--
makeFollowupAfterIndex
  :: (Followup i a, IntervalSizeable a b)
  => b
  -> b
  -> Index i a
  -> AssessmentInterval a
makeFollowupAfterIndex shiftBy dur index = Fl (followupAfter shiftBy dur index)
