{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- TODO: why is this needed?
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Functions for Parsing Events from JSON lines
-- Description : Defines FromJSON instances for Events.
-- Copyright   : (c) Target RWE 2023
-- License     : BSD3
-- Maintainer  : bbrown@targetrwe.com
--               ljackman@targetrwe.com
--               dpritchard@targetrwe.com
module EventDataTheory.EventLines
  ( EventLine (..),
    EventLineAble,
    parseEventLinesL',
    eitherDecodeEvent',
    decodeEvent',
    LineParseError (..),
    ParseEventLineOption (..),
    defaultParseEventLineOption,
    modifyEventLineWithContext,
    -- for internal use;
    FactsLine (..),
    TimeLine (..),
  )
where

import Control.Exception
import Control.Monad ((>=>))
import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import Data.Data
import Data.Either (Either (..), partitionEithers)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text, pack)
import EventDataTheory.Core
import GHC.Generics (Generic)
import GHC.Num (Integer, Natural)
import IntervalAlgebra
  ( Interval,
    ParseErrorInterval,
    SizedIv (..),
    begin,
    beginerval,
    beginervalMoment,
    end,
    getInterval,
    parseInterval,
  )
import Type.Reflection (Typeable)
import Witch

-- |
-- At this time,
-- 'EventLine', 'FactsLine', and 'TimeLine' are
-- simply wrapper types
-- in order to create 'FromJSON' instances which can be used to marshal data from
-- [ndjson](http://ndjson.org/).
--
-- See [event data model docs](https://docs.novisci.com/event-data/3.0/index.html)
data EventLine t m a = MkEventLine Value Value Value Value [t] (FactsLine m a)
  deriving (Eq, Show, Generic)

instance
  (FromJSONEvent t m a) =>
  FromJSON (EventLine t m a)

instance (ToJSON a, ToJSON t, ToJSON m) => ToJSON (EventLine t m a)

-- TODO: check uses of this synonym and decide if the constraints are really needed.
-- if appropriate, come back and delete this.

-- | A synonym for the basic class constraints needed to create an @EventLine@.
type EventLineAble t m a b =
  (Generic m, Typeable m, Typeable t, Typeable a, SizedIv (Interval a))

-- INTERNAL utility for getting subjectID from EventLine
getSubjectID :: EventLine t m a -> SubjectID
getSubjectID (MkEventLine _ _ _ _ _ fcts) = patient_id fcts

-- INTERNAL utility for getting the FactsLine from EventLine
fctln :: EventLine t m a -> FactsLine m a
fctln (MkEventLine _ _ _ _ _ x) = x

-- INTERNAL utility for getting a tag set from EventLine
tagSetIn :: EventLine t m a -> [t]
tagSetIn (MkEventLine _ _ _ _ x _) = x

-- |
-- Options for how an 'EventLine' will be parsed into an 'Event'.
data ParseEventLineOption
  = -- | Add a 'moment' to the 'end' of all intervals,
    -- so long as one is provided and 'end x >= begin x'.
    -- In particular, this option creates a moment-length interval
    -- in the case where 'end x == begin x', which otherwise would
    -- fail to parse via @IntervalAlgebra.'parseInterval'@.
    --
    -- It does not modify intervals for which the provided end is null,
    -- which will fail to parse. See 'FixEnd' and 'AddMomentAndFix'.
    AddMomentToEnd
  | -- | Do not modify the @TimeLine@ before
    --   trying to 'IntervalAlgebra.parseInterval'.
    DoNotModifyTime
  | -- | Convert @TimeLine@ with @timeEnd@ as @Nothing@ to
    -- 'moment' - length intervals. Otherwise attempt to parse the
    -- interval as-is. An important difference with 'AddMomentToEnd'
    -- is that cases where timeEnd == timeBegin are unchanged here and
    -- will fail to parse.
    FixEnd
  | -- | Apply fixes from both 'FixEnd' and 'AddMomentToEnd'.
    AddMomentAndFix
  deriving (Show)

-- | The default 'ParseEventLineOption'.
defaultParseEventLineOption :: ParseEventLineOption
defaultParseEventLineOption = DoNotModifyTime

instance Exception ParseErrorInterval

-- | See 'EventLine'.
data FactsLine m a = MkFactsLine
  { time :: TimeLine a,
    facts :: m,
    patient_id :: SubjectID,
    source :: Maybe Source,
    valid :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance
  (FromJSON a, FromJSON m) =>
  FromJSON (FactsLine m a)

instance (ToJSON a, ToJSON m) => ToJSON (FactsLine m a)

-- | See 'EventLine'.
data TimeLine a = MkTimeLine
  { timeBegin :: a,
    timeEnd :: Maybe a
  }
  deriving (Eq, Show)

instance (FromJSON a) => FromJSON (TimeLine a) where
  parseJSON = withObject "TimeLine" $ \o -> do
    b <- o .: "begin"
    e <- o .:? "end"
    pure (MkTimeLine b e)

instance (ToJSON a) => ToJSON (TimeLine a) where
  toJSON (MkTimeLine b e) = case e of
    Nothing -> object ["begin" .= b]
    Just x -> object ["begin" .= b, "end" .= x]

{- DECODING / PARSING -}

-- |
-- Decode a bytestring corresponding to an 'EventLine' into
-- @Either String (SubjectID, Event t m a)@,
-- where the @String@ is an error message on failure
-- and @(SubjectID, Event t m a)@ is the success case.
--
-- NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22
-- for discusson of json vs json'.
eitherDecodeEvent' ::
  forall m t a b.
  (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a) =>
  ParseEventLineOption ->
  C.ByteString ->
  Either String (SubjectID, Event t m a)
eitherDecodeEvent' opt = eitherDecodeStrict' @(EventLine t m a) >=> convertOp opt
  where
    convertOp opt' eline =
      --- Convert the TryFrom error into a String
      first show $
        -- Attach the subject id to the Right result of tryInto
        (getSubjectID eline,)
          <$> tryInto @(Event t m a) (eline, opt')

-- |
-- Decode a bytestring corresponding to an 'EventLine' into
-- @Maybe (SubjectID, Event t m a)@,
-- where the value is @Nothing@ on failure
-- and @Just (SubjectID, Event t m a)@ on success.
--
-- NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22
-- for discusson of json vs json'.
decodeEvent' ::
  forall m t a b.
  (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a) =>
  ParseEventLineOption ->
  C.ByteString ->
  Maybe (SubjectID, Event t m a)
decodeEvent' opt = rightToMaybe . eitherDecodeEvent' opt

-- | INTERNAL utlity for transforming an @Either@ into a @Maybe@.
rightToMaybe :: Either b a -> Maybe a
rightToMaybe (Left _) = Nothing
rightToMaybe (Right x) = Just x

-- |
-- Contains the line number and error message of any parsing errors.
newtype LineParseError = MkLineParseError (Natural, String)
  deriving (Eq, Show, Generic)

-- providing a From instance making LineParseError values in the tests
instance From (Natural, String) LineParseError

-- |
-- Parse @Event t m a@ from new-line delimited JSON.
--
-- Per the [aeson docs](https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22),
-- when using this version:
-- This is a strict version of json which avoids building up thunks during parsing;
-- it performs all conversions immediately.
-- Prefer this version if most of the JSON data needs to be accessed.
--
-- Returns a pair where
-- the first element is a list of parse errors
-- and the second element is a list of successfully parsed (subjectID, event) pairs.
--
-- Note the input must be UTF-8.
parseEventLinesL' ::
  forall m t a b.
  (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a) =>
  ParseEventLineOption ->
  C.ByteString ->
  ([LineParseError], [(SubjectID, Event t m a)])
parseEventLinesL' opt l =
  partitionEithers $
    zipWith
      (\x i -> first (\t -> MkLineParseError (i, t)) (eitherDecodeEvent' opt x))
      (C.lines l)
      [1 ..]

{- CONVERSION TO EVENTS -}

-- |
-- Try to parse an @'EventLine'@ into an @'Event'@,
-- given an 'ParseEventLineOption'.
instance
  (SizedIv (Interval a), Eventable t m a) =>
  TryFrom (EventLine t m a, ParseEventLineOption) (Event t m a)
  where
  tryFrom (eline, opt) = flip event ctx <$> parseIv opt i
    where
      fcts = fctln eline
      i = time fcts
      ctx = context (from @[t] $ tagSetIn eline) (facts fcts) (source fcts)
      -- Convert parseInterval errors to the more informative
      -- TryFromException.
      toTryFromException = TryFromException (eline, opt)
      tryParse b e = first (toTryFromException . Just . toException) $ parseInterval b e
      addMoment b' e' =
        if b' == e'
          then Right $ beginervalMoment b'
          else ivExpandr (moment @(Interval a)) <$> tryParse b' e'
      parseIv AddMomentToEnd iv = case timeEnd iv of
        Nothing -> Left $ toTryFromException Nothing
        Just e -> addMoment (timeBegin iv) e
      parseIv AddMomentAndFix iv = case timeEnd iv of
              Nothing -> Right $ beginervalMoment $ timeBegin iv
              Just e -> addMoment (timeBegin iv) e
      parseIv DoNotModifyTime iv =
        maybe (Left $ toTryFromException Nothing) (tryParse (timeBegin iv)) $
          timeEnd iv
      parseIv FixEnd iv =
        maybe (Right $ beginervalMoment (timeBegin iv)) (tryParse (timeBegin iv)) $
          timeEnd iv

{-------------------------------------------------------------------------------
Transforming event lines

-- TODO: it's unclera whether we need any of the remaining code of this module.
-- see #402.
-------------------------------------------------------------------------------}
{-
INTERNAL

Modify a @FactsLine@ value with values from a @Context@.
The @TimeLine@ value IS NOT changed.
Only those fields in the context that align with the factsline
are modified.
-}
updateFactsLine :: FactsLine m a -> Context t m' -> FactsLine m' a
updateFactsLine (MkFactsLine tm _ sid _ vld) x =
  MkFactsLine
    { time = tm,
      facts = getFacts x,
      patient_id = sid,
      source = getSource x,
      valid = vld
    }

{-
INTERNAL

Modify a @FactsLine@ value with values from a @Context@.
The @TimeLine@ value IS NOT changed
based on the provided interval.
Only those fields in the context that align with the factsline
are modified.
-}
updateFactsLineWithInterval ::
  -- TODO: this can be downgraded to a PointedIv constraint when this is done:
  -- https://gitlab.com/TargetRWE/epistats/nsstat/interval-algebra/-/issues/142
  (SizedIv (Interval a')) =>
  FactsLine m a ->
  Context t m' ->
  Interval a' ->
  FactsLine m' a'
updateFactsLineWithInterval (MkFactsLine _ _ sid _ vld) x i =
  MkFactsLine
    { time = MkTimeLine (begin i) (Just $ end i),
      facts = getFacts x,
      patient_id = sid,
      source = getSource x,
      valid = vld
    }

{-
INTERNAL

Modifies data in an @EventLine@
from data in an @Event@.
-}
updateEventLineFromEvent ::
  (Data m', SizedIv (Interval a'), ToJSON a', Ord t') =>
  EventLine t m a ->
  Event t' m' a' ->
  EventLine t' m' a'
updateEventLineFromEvent (MkEventLine _ _ _ _ _ f) x =
  let ctxt = getContext x
   in let i = getInterval x
       in MkEventLine
            (toJSON (patient_id f))
            (toJSON (begin i))
            (toJSON (Just . end $ i))
            (toJSON (show . toConstr . getFacts $ ctxt))
            (into (getTagSet ctxt))
            (updateFactsLineWithInterval f ctxt i)

{-
INTERNAL

Transforms an Eventline via a function
that operates on the Context
within the Event corresponding to the EventLine.
-}
eitherModifyEventLineFromContext ::
  forall m m' t t' a b e.
  ( Eventable t m a,
    EventLineAble t m a b,
    FromJSONEvent t m a,
    Ord t',
    Data m'
  ) =>
  ParseEventLineOption ->
  (Context t m -> Context t' m') ->
  EventLine t m a ->
  Either String (EventLine t' m' a)
eitherModifyEventLineFromContext opt g (MkEventLine a b t m e f) = do
  ev <- first show $ tryInto @(Event t m a) (MkEventLine a b t m e f, opt)
  let ctxt = g (getContext ev)
  let newFl = updateFactsLine f ctxt
  pure $ MkEventLine a b t m (into . getTagSet $ ctxt) newFl

{-
TODO
-}
eitherModifyEventLineFromEvent ::
  forall m m' t t' a a' b e.
  ( Eventable t m a,
    Eventable t' m' a',
    EventLineAble t m a b,
    SizedIv (Interval a'),
    FromJSONEvent t m a,
    ToJSON a',
    Data m'
  ) =>
  ParseEventLineOption ->
  (Event t m a -> Event t' m' a') ->
  EventLine t m a ->
  Either String (EventLine t' m' a')
eitherModifyEventLineFromEvent opt g x = do
  ev1 <- first show $ tryInto @(Event t m a) (x, opt)
  let ev2 = g ev1
  pure $ updateEventLineFromEvent x ev2

-- |
-- This function:
--
-- * parses a JSON bytestring into an 'EventLine'
-- * modifies the data in the 'EventLine'
-- that corresponds to an 'Event' 'Context'
-- using the supplied function
--
-- The function may fail and return an error message
-- if either the JSON parsing fails
-- or the 'EventLine' -> 'Event' tranformation fails.
--
-- This function does not modify the time information in the 'EventLine',
-- nor any of the first four elements of the 'EventLine'.
--
-- See 'modifyEventLineWithEvent' for a function that can also modify the interval.
modifyEventLineWithContext ::
  forall m m' t t' a b.
  ( Eventable t m a,
    EventLineAble t m a b,
    FromJSONEvent t m a,
    Eventable t' m' a,
    Data m'
  ) =>
  ParseEventLineOption ->
  (Context t m -> Context t' m') ->
  B.ByteString ->
  Either String (EventLine t' m' a)
modifyEventLineWithContext opt f x =
  let el = eitherDecode @(EventLine t m a) x
   in eitherModifyEventLineFromContext opt f =<< el

{-
NOT EXPORTED AT THIS TIME
HERE FOR FURTHER CONSIDERATION

This function:

\* parses a JSON bytestring into an 'EventLine'
\* modifies the data in the 'EventLine'
that corresponds to an 'Event'
using the supplied function

The function may fail and return an error message
if either the JSON parsing fails
or the 'EventLine' -> 'Event' tranformation fails.

This function may modify time information in the 'EventLine',
thus cannot be used to roundtrip to/from JSON isomorphically.
For example, if the end of an interval is missing in the input JSON,
the output will contain an interval end if the 'AddMomentToTimeEnd'
parse option was used.

Therefore, USER BEWARE.

-}
modifyEventLineWithEvent ::
  forall m m' t t' a a' b.
  ( FromJSONEvent t m a,
    Eventable t m a,
    Eventable t' m' a',
    SizedIv (Interval a'),
    EventLineAble t m a b,
    ToJSON a',
    Data m'
  ) =>
  ParseEventLineOption ->
  (Event t m a -> Event t' m' a') ->
  B.ByteString ->
  Either String (EventLine t' m' a')
modifyEventLineWithEvent opt f x =
  let el = eitherDecode @(EventLine t m a) x
   in eitherModifyEventLineFromEvent opt f =<< el
